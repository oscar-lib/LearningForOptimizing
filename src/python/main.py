from py4j.java_gateway import JavaGateway, CallbackServerParameters


class PythonListener(object):
    def __init__(self, gateway):
        self.gateway = gateway

    def notify(self, obj: int):
        print("Notified by Java")
        print(obj)
        gateway.jvm.System.out.println("Hello from python!")

        return "A Return Value"

    class Java:
        implements = ["bridge.PythonInterface"]


if __name__ == "__main__":
    server = Runner(5555)
    server.run()
