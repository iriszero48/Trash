import http = require("http");
http.createServer((req, res) => {
    res.writeHead(200, { 'Content-Type': "text/plain" });
    res.end("Goodbye, World!");
}).listen(18851, "127.0.0.1");
