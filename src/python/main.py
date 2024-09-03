from runner import Runner

if __name__ == "__main__":
    with open("/tmp/ipc-scala2py", "rb") as pipe:
        while True:
            c = pipe.read(1)
            if len(c) == 0:
                exit(0)
            print(c)

    server = Runner(5555)
    server.run()
