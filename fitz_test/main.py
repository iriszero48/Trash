import json
import time
import uuid
import hashlib
import fitz
import socketserver


if __name__ == '__main__':
    print(fitz.__doc__)

    doc = fitz.open('')
    with open('', 'w') as f:
        f.write(doc[0].f('html'))

    exit(0)

    with open('test.xml', 'w') as f:
        f.write(doc.get_xml_metadata())

    page = doc[7]
    paths = page.get_drawings()
    outpdf = fitz.open()
    outpage = outpdf.new_page(width=page.rect.width, height=page.rect.height)
    shape = outpage.new_shape()
    for path in paths:
        print(path)
        for item in path["items"]:
            if item[0] == "l":
                shape.draw_line(item[1], item[2])
            elif item[0] == "re":
                shape.draw_rect(item[1])
            elif item[0] == "c":
                shape.draw_bezier(item[1], item[2], item[3], item[4])
            else:
                raise ValueError("unhandled drawing", item)

        shape.finish(
            fill=path["fill"], 
            color=path["color"], 
            dashes=path["dashes"],  
            even_odd=path["even_odd"],  
            closePath=path["closePath"],  
            lineJoin=path["lineJoin"],  
            lineCap=max(path["lineCap"]), 
            width=path["width"], 
            stroke_opacity=path["opacity"],  
            fill_opacity=path["opacity"], 
        )

    shape.commit()
    with open('test.svg', 'w') as f:
        f.write(outpdf[0].get_svg_image(None, 1))

    class ImgEncoder(json.JSONEncoder):
        def default(self, obj):
            if isinstance(obj, bytes):
                filename = f'{uuid.uuid4()}'
                with open(filename, 'wb') as f:
                    f.write(obj)
                return filename
            return json.JSONEncoder.default(self, obj)

    #with open('test.json', 'w') as fs:
    #    val = doc[6 - 1].get_text('dict')
    #    print(val)
    #    val = json.dumps(val, separators=(",", ":"), cls=ImgEncoder, indent=1)
    #    fs.write(val)
    #with open('test.html', 'w') as fs:
    #    fs.write(doc[6-1].get_text('html'))
    #with open('test.html', 'w') as fs:
    #    for page in doc:
    #        fs.write(page.get_text('html'))

    class Srv(socketserver.BaseRequestHandler):
        def handle(self):
            while True:
                conn = self.request
                addr = self.client_address
                print(addr)
                recv_data = str(conn.recv(4096), encoding='utf8')
                print(recv_data)
                url = recv_data.split(' ')[1]
                if url.startswith('/query'):
                    raw_params = url.split('?')[1].split('&')
                    params = {}
                    for i in raw_params:
                        kv = i.split('=')
                        params[kv[0]] = kv[1]
                    t1 = time.time()
                    doc = fitz.open(params['file'])
                    send_data = json.dumps(doc[int(params['pos'])].get_text('dict'), separators=(",", ":"), cls=ImgEncoder)
                    t2 = time.time()
                    print(t2 - t1)
                    resp = f"""HTTP/1.1 200 OK\r
Connection: close\r
Content-Length: {len(send_data)}\r
Server: PyShit/0.0.1\r
\r
{send_data}"""
                    conn.sendall(resp.encode('utf-8'))
                    #conn.close()
                elif url.startswith('/index.html'):
                    with open(f'{url}', 'rb') as f:
                        dt = f.read()
                        resp = f"""HTTP/1.1 200 OK\r
Connection: close\r
Content-Length: {len(dt)}\r
Server: PyShit/0.0.1\r
\r
"""
                        conn.send(resp.encode('utf-8'))
                        conn.send(dt)
                    #conn.close()
                    continue
                else:
                    with open(f'{url}', 'rb') as f:
                        dt = f.read()
                        resp = f"""HTTP/1.1 200 OK\r
Connection: close\r
Content-Length: {len(dt)}\r
Server: PyShit/0.0.1\r
\r
"""
                        conn.send(resp.encode('utf-8'))
                        conn.send(dt)
                    #conn.close()
                    continue

    server = socketserver.ThreadingTCPServer(('127.0.0.1', 10002), Srv)
    server.serve_forever()

