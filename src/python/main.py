from runner import Runner
from dotenv import load_dotenv
import logging
import os


if __name__ == "__main__":
    load_dotenv()
    level = os.environ.get("LOG_LEVEL", None)
    if level:
        level = level.upper()
        print(f"Setting log level to {level}")
        logging.basicConfig(level=level)

    server = Runner()
    server.run()
