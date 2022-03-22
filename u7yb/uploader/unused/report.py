if __name__ == '__main__':
    total = 0
    pre = 0
    speed = 0
    dt = []

    done = []

    all_skip = sum(done) + sum(dt)

    print(f'{all_skip}，{all_skip / total * 100}%，{(total - pre - sum(dt)) / speed}')
