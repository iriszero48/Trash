package FuckServlet;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.net.URLDecoder;

@WebServlet("/Fuck")
public class Servlet extends HttpServlet
{
    private static final int ThreadNum = 100;
    private static final String DatabasePath = "shitlet.db";
    private Map<String, String> database = new HashMap<>();
    private List<String> history = new LinkedList<>();
    private List<Thread> pool = new ArrayList<>();
    private List<OutputStream> clients = new ArrayList<>();
    private Lock lock = new ReentrantLock();
    private ServerSocket ws;

    private void UpdateMessage(String msg)
    {
        lock.lock();
        System.out.println(msg);
        history.add(msg);
        byte[] data = WebSocketEncode(msg);
        clients.forEach((x) ->
        {
            if (x != null)
            {
                try
                {
                    x.write(data);
                }
                catch (IOException e)
                {
                    log(e.getMessage());
                }
            }
        });
        lock.unlock();
    }

    private void SyncMessage(String msg, int id)
    {
        try
        {
            clients.get(id).write(WebSocketEncode(msg));
        }
        catch (IOException e)
        {
            log(e.getMessage());
        }
    }

    private byte[] SHA1Hash(byte[] bytes) throws NoSuchAlgorithmException
    {
        MessageDigest md = MessageDigest.getInstance("SHA-1");
        md.update(bytes);
        return md.digest();
    }

    private String SecWebSocketAccept(String key)
    {
        try
        {
            return new String(Base64.getEncoder().encode((SHA1Hash((key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").getBytes()))));
        }
        catch (Exception e)
        {
            e.printStackTrace();
            return "";
        }
    }

    public String WebSocketDecode(byte[] raw, int len)
    {
        byte rLength = 0;
        int rMaskIndex = 2;
        int rDataStart = 0;
        byte data = raw[1];
        byte op = (byte) 127;
        rLength = (byte) (data & op);
        if (rLength == (byte) 126) rMaskIndex = 4;
        if (rLength == (byte) 127) rMaskIndex = 10;
        byte[] masks = new byte[4];
        int j = 0;
        int i = 0;
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

    private byte[] WebSocketEncode(String raw)
    {
        byte[] rawData = raw.getBytes(StandardCharsets.UTF_8);
        int frameCount = 0;
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
            frame[2] = (byte) ((len >> 56) & (byte) 255);
            frame[3] = (byte) ((len >> 48) & (byte) 255);
            frame[4] = (byte) ((len >> 40) & (byte) 255);
            frame[5] = (byte) ((len >> 32) & (byte) 255);
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

    private void LoadDatabase(Map<String, String> db) throws IOException
    {
        BufferedReader br = new BufferedReader(new FileReader(DatabasePath));
        String line;
        while ((line = br.readLine()) != null)
        {
            String[] up = line.split(" ");
            db.put(up[0], up[1]);
        }
        br.close();
    }

    private void UpdateDatabase(Map<String, String> db, String[] up) throws IOException
    {
        lock.lock();
        database.put(up[1], up[2]);
        FileWriter fw = new FileWriter(new File(DatabasePath), true);
        fw.write(Arrays.stream(up).skip(1).reduce((a, b) -> a + ' ' + b).get() + "\n");
        fw.close();
        lock.unlock();
    }

    private String CheckUser(String[] up)
    {
        Arrays.stream(up).forEach(System.out::println);
        try
        {
            if (up.length != 3
                    || Base64.getDecoder().decode(up[1]).length > 30
                    || !up[2].matches("^[A-Za-z0-9]{32}$"))
            {
                return "Invalid Username/Password";
            }
            if (up[0].equals("l") && database.containsKey(up[1]) && up[2].equals(database.get(up[1])))
            {
                return "true";
            }
            if (up[0].equals("r") && !database.containsKey(up[1]))
            {
                UpdateDatabase(database, up);
                return "true";
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        return "Invalid Username/Password";
    }

    @Override
    public void init() throws ServletException
    {
        super.init();
        try
        {
            LoadDatabase(database);
        }
        catch (IOException e)
        {
            try
            {
                new File(DatabasePath).createNewFile();
            }
            catch (IOException ex)
            {
                ex.printStackTrace();
            }
        }
        try
        {
            ws = new ServerSocket(27015);
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        for (int i = 0; i < ThreadNum; ++i)
        {
            clients.add(null);
        }
        for (int i = 0; i < ThreadNum; ++i)
        {
            int id = i;
            pool.add(new Thread(() ->
            {
                while (true)
                {
                    try
                    {
                        Socket sock = ws.accept();
                        InputStream inputStream = sock.getInputStream();
                        OutputStream outputStream = sock.getOutputStream();
                        clients.set(id, outputStream);
                        byte[] buf = new byte[4096];
                        int len = 0;
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
                                            SecWebSocketAccept(
                                                    request
                                                            .toString()
                                                            .split("Sec-WebSocket-Key: ")[1]
                                                            .split("\r\n")[0]) +
                                            "\r\n\r\n").getBytes());
                            len = inputStream.read(buf, 0, 4096);
                            String[] up = WebSocketDecode(buf, len).split(" ");
                            String msg = CheckUser(up);
                            outputStream.write(WebSocketEncode(msg));
                            if (msg.equals("true") && up[0].equals("l"))
                            {
                                history.forEach((x) -> SyncMessage(x, id));
                                while (true)
                                {
                                    len = inputStream.read(buf, 0, 4096);
                                    if (len > 0)
                                    {
                                        UpdateMessage(
                                                "<b class=\"un\">" +
                                                        URLDecoder.decode(
                                                                new String(Base64.getDecoder().decode(up[1])),
                                                                StandardCharsets.UTF_8) + "</b>:&nbsp;&nbsp;&nbsp;&nbsp;" +
                                                        WebSocketDecode(buf, len)
                                                                .replace("<","&lt;")
                                                                .replace(">", "&gt;")
                                                                .replace(" ", "&nbsp;") +
                                                        "<sub>&nbsp;&nbsp;&nbsp;&nbsp;" +
                                                        new java.util.Date().toString() + "</sub>");
                                    }
                                }
                            }
                            sock.close();
                            clients.set(id, null);
                        }
                        else
                        {
                            sock.close();
                            clients.set(id, null);
                        }
                    }
                    catch (IOException e)
                    {
                        e.printStackTrace();
                        clients.set(id, null);
                    }
                }
            }));
            pool.get(id).start();
        }
    }

    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
    {

    }

    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
    {
        response.setContentType("text/html");
        response.getWriter().println("<h1>Fuck Servlet</h1>");
    }
}
