import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.net.URLDecoder;
import java.util.stream.IntStream;

class Database
{
    private Lock lock = new ReentrantLock();
    String path;

    Database(String path)
    {
        this.path = path;
    }

    List<String> Load() throws IOException
    {
        try
        {
            return Files.readAllLines(Paths.get(path), StandardCharsets.UTF_8);
        }
        catch (IOException e)
        {
            if (!new File(path).createNewFile()) throw new IOException();
            return new LinkedList<>();
        }
    }

    void AppendLine(String data) throws IOException
    {
        lock.lock();
        try
        {
            FileWriter fw = new FileWriter(new File(path), true);
            fw.write(data + "\n");
            fw.close();
        }
        catch (IOException e)
        {
            if (!new File(path).createNewFile()) throw new IOException();
            Files.writeString(Paths.get(path), data + "\n");
        }
        lock.unlock();
    }
}

class UserDatabase extends Database
{
    UserDatabase(String path)
    {
        super(path);
    }

    void Load(Map<String, String> md) throws IOException
    {
        try
        {
            BufferedReader br = new BufferedReader(new FileReader(path));
            String line;
            while ((line = br.readLine()) != null)
            {
                String[] up = line.split(" ");
                md.put(up[0], up[1]);
            }
            br.close();
        }
        catch (FileNotFoundException e)
        {
            if (!new File(path).createNewFile()) throw new IOException();
        }
    }
}

class LogDatabase extends Database
{
    LogDatabase(String path)
    {
        super(path);
    }
}

class WebSocket
{
    private static byte[] SHA1Hash(byte[] bytes) throws NoSuchAlgorithmException
    {
        MessageDigest md = MessageDigest.getInstance("SHA-1");
        md.update(bytes);
        return md.digest();
    }

    static String Decode(byte[] raw, int len)
    {
        byte rLength;
        int rMaskIndex = 2;
        int rDataStart;
        byte data = raw[1];
        byte op = (byte) 127;
        rLength = (byte) (data & op);
        if (rLength == (byte) 126) rMaskIndex = 4;
        if (rLength == (byte) 127) rMaskIndex = 10;
        byte[] masks = new byte[4];
        int j = 0;
        int i;
        for (i = rMaskIndex; i < (rMaskIndex + 4); i++)
        {
            masks[j] = raw[i];
            j++;
        }
        rDataStart = rMaskIndex + 4;
        int messLen = len - rDataStart;
        byte[] message = new byte[messLen];
        for (i = rDataStart, j = 0; i < len; i++, j++)
        {
            message[j] = (byte) (raw[i] ^ masks[j % 4]);
        }
        return new String(message, StandardCharsets.UTF_8);
    }

    static byte[] Encode(String raw)
    {
        byte[] rawData = raw.getBytes(StandardCharsets.UTF_8);
        int frameCount;
        byte[] frame = new byte[10];
        frame[0] = (byte) 129;
        if (rawData.length <= 125)
        {
            frame[1] = (byte) rawData.length;
            frameCount = 2;
        }
        else if (rawData.length <= 65535)
        {
            frame[1] = (byte) 126;
            int len = rawData.length;
            frame[2] = (byte) ((len >> 8) & (byte) 255);
            frame[3] = (byte) (len & (byte) 255);
            frameCount = 4;
        }
        else
        {
            frame[1] = (byte) 127;
            int len = rawData.length;
            frame[2] = (byte) ((len >> 24) & (byte) 255);
            frame[3] = (byte) ((len >> 16) & (byte) 255);
            frame[4] = (byte) ((len >> 8) & (byte) 255);
            frame[5] = (byte) ((len) & (byte) 255);
            frame[6] = (byte) ((len >> 24) & (byte) 255);
            frame[7] = (byte) ((len >> 16) & (byte) 255);
            frame[8] = (byte) ((len >> 8) & (byte) 255);
            frame[9] = (byte) (len & (byte) 255);
            frameCount = 10;
        }
        int bLength = frameCount + rawData.length;
        byte[] reply = new byte[bLength];
        int bLim = 0;
        for (int i = 0; i < frameCount; i++)
        {
            reply[bLim] = frame[i];
            bLim++;
        }
        for (byte rawDatum : rawData)
        {
            reply[bLim] = rawDatum;
            bLim++;
        }
        return reply;
    }

    static String SecWebSocketAccept(String key) throws NoSuchAlgorithmException
    {
        return new String(Base64.getEncoder().encode((SHA1Hash((key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").getBytes()))));
    }
}


class ChatServer
{
    private Lock lock = new ReentrantLock();
    private LogDatabase ld;
    private UserDatabase ud;
    private List<Client> clients = new ArrayList<>();
    private ServerSocket ws;
    private int threadNum;

    ChatServer(int port, int threadNum, String logDatabasePath, String userDatabasePath) throws IOException
    {
        ws = new ServerSocket(port);
        ld = new LogDatabase(logDatabasePath);
        ud = new UserDatabase(userDatabasePath);
        this.threadNum = threadNum;
    }

    private void UpdateMessage(String msg) throws IOException
    {
        lock.lock();
        ld.AppendLine(msg);
        byte[] data = WebSocket.Encode(msg);
        clients.forEach((x) ->
        {
            if (x.outputStream != null)
            {
                try
                {
                    x.Write(data);
                }
                catch (IOException e)
                {
                    e.printStackTrace(System.err);
                }
            }
        });
        lock.unlock();
    }

    private void SyncMessage(String msg, int id)
    {
        try
        {
            clients.get(id).Write(WebSocket.Encode(msg));
        }
        catch (IOException e)
        {
            e.printStackTrace(System.err);
        }
    }

    private String CheckUser(String[] up) throws IOException
    {
        Map<String, String> md = new HashMap<>();
        ud.Load(md);
        if (up.length != 3 || !up[2].matches("^[A-Za-z0-9]{32}$"))
        {
            return "Invalid Username/Password";
        }
        try
        {
            if (Base64.getDecoder().decode(up[1]).length > 30)
            {
                return "Invalid Username/Password";
            }
        }
        catch (Exception e)
        {
            e.printStackTrace(System.err);
        }
        if (up[0].equals("l") && md.containsKey(up[1]) && up[2].equals(md.get(up[1])))
        {
            return "true";
        }
        if (up[0].equals("r") && !md.containsKey(up[1]))
        {
            ud.AppendLine(Arrays.stream(up).skip(1).reduce((a, b) -> a + " " + b).get());
            return "true";
        }
        return "Invalid Username/Password";
    }

    void Start()
    {
        IntStream.range(0, threadNum).forEach(id -> clients.add(new Client(id)));
        clients.forEach(Client::Start);
    }

    void Wait()
    {
        clients.forEach(Client::Wait);
    }

    class Client
    {
        private Thread thread;
        private OutputStream outputStream = null;

        Client(int id)
        {
            String end = new String(
                    new byte[]{0x03, (byte) 0xef, (byte) 0xbf, (byte) 0xbd},
                    StandardCharsets.UTF_8);
            thread = new Thread(() ->
            {
                while (true)
                {
                    try
                    {
                        Socket sock = ws.accept();
                        InputStream inputStream = sock.getInputStream();
                        outputStream = sock.getOutputStream();
                        byte[] buf = new byte[4096];
                        int len;
                        StringBuilder request = new StringBuilder();
                        while ((len = inputStream.read(buf, 0, 4096)) == 4096)
                            request.append(new String(buf, 0, 4096));
                        request.append(new String(buf, 0, len));
                        if (request.toString().contains("Upgrade: websocket"))
                        {
                            outputStream.write((
                                    "HTTP/1.1 101 Switching Protocols\r\n" +
                                            "Upgrade: websocket\r\n" +
                                            "Connection: Upgrade\r\n" +
                                            "Sec-WebSocket-Accept: " +
                                            WebSocket.SecWebSocketAccept(
                                                    request
                                                            .toString()
                                                            .split("Sec-WebSocket-Key: ")[1]
                                                            .split("\r\n")[0]) +
                                            "\r\n\r\n").getBytes());
                            len = inputStream.read(buf, 0, 4096);
                            String[] up = WebSocket.Decode(buf, len).split(" ");
                            System.out.println(String.join(" ", up));
                            String msg = CheckUser(up);
                            outputStream.write(WebSocket.Encode(msg));
                            if (msg.equals("true") && up[0].equals("l"))
                            {
                                ld.Load().forEach(x -> SyncMessage(x, id));
                                while (true)
                                {
                                    len = inputStream.read(buf, 0, 4096);
                                    String message = WebSocket.Decode(buf, len)
                                                    .replace("<", "&lt;")
                                                    .replace(">", "&gt;")
                                                    .replace(" ", "&nbsp;");
                                    if (!message.equals(end))
                                    {
                                        String send = "<b class=\"un\">" +
                                                URLDecoder.decode(
                                                        new String(Base64.getDecoder().decode(up[1])),
                                                        StandardCharsets.UTF_8) +
                                                "</b>:&nbsp;&nbsp;&nbsp;&nbsp;" +
                                                message +
                                                "<sub>&nbsp;&nbsp;&nbsp;&nbsp;" +
                                                new java.util.Date().toString() + "</sub>";
                                        System.out.println(send);
                                        UpdateMessage(send);
                                    }
                                }
                            }
                            sock.close();
                            outputStream = null;
                        }
                        else
                        {
                            sock.close();
                            outputStream = null;
                        }
                    }
                    catch (Exception e)
                    {
                        e.printStackTrace(System.err);
                    }
                }
            });
        }

        void Write(byte[] data) throws IOException
        {
            outputStream.write(data);
        }

        void Start()
        {
            thread.start();
        }

        void Wait()
        {
            try
            {
                thread.join();
            }
            catch (InterruptedException e)
            {
                e.printStackTrace(System.err);
            }
        }
    }
}

class WebServer
{
    private List<Thread> pool = new ArrayList<>();
    private ServerSocket ss;

    WebServer(String webPath, int port, int threadNum) throws IOException
    {
        ss = new ServerSocket(port);
        String webPage = Files.readString(Paths.get(webPath));
        int len = webPage.getBytes(StandardCharsets.UTF_8).length;
        byte[] http = ("HTTP/1.1 200 OK\r\n" +
                "Content-Length: " + len +
                "\r\nServer: iriszero/AjChat/1.0" +
                "\r\nContent-Type: text/html\r\n\r\n" +
                webPage).getBytes(StandardCharsets.UTF_8);
        IntStream.range(0, threadNum).forEach(i ->
                pool.add(new Thread(() ->
                {
                    byte[] buf = new byte[4096];
                    while (true)
                    {
                        try
                        {
                            Socket sock = ss.accept();
                            int bufLen = sock.getInputStream().read(buf);
                            System.out.println("<----------" +
                                    sock.getRemoteSocketAddress().toString() + "\n" +
                                    new String(buf,0, bufLen, StandardCharsets.UTF_8));
                            sock.getOutputStream().write(http);
                            sock.close();
                        }
                        catch (Exception e)
                        {
                            e.printStackTrace(System.err);
                        }
                    }
                })));
    }

    void Start()
    {
        pool.forEach(Thread::start);
    }

    void Wait()
    {
        pool.forEach(t ->
        {
            try
            {
                t.join();
            }
            catch (InterruptedException e)
            {
                e.printStackTrace(System.err);
            }
        });
    }
}

public class Main
{
    public static void main(String[] argv) throws IOException
    {
        if (argv.length == 7)
        {
            WebServer ws = new WebServer(argv[0], Integer.parseInt(argv[1]), Integer.parseInt(argv[2]));
            ChatServer cs = new ChatServer(Integer.parseInt(argv[3]), Integer.parseInt(argv[4]), argv[5], argv[6]);
            ws.Start();
            cs.Start();
            ws.Wait();
            cs.Wait();
        }
        else
        {
            System.err.println("./main IndexFilePath IndexPort IndexThreadNum " +
                    "WebSocketPort WebSocketThreadNum LogDatabasePath UserDatabasePath");
        }
    }
}
