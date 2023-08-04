import logging
import os

HERE = os.path.dirname(__file__)
LOGFILE = os.path.join(HERE, "logfile.txt")

console_handler = logging.StreamHandler()
console_handler.setLevel(logging.DEBUG)
file_handler = logging.FileHandler(LOGFILE)
file_handler.setLevel(logging.INFO)

logging.basicConfig(
    level=logging.DEBUG,
    # format='%(asctime)s %(message)s',
    format=(
        '[%(asctime)s]'
        # ' {%(pathname)s:%(lineno)d}'
        ' %(levelname)s'
        ' - %(message)s'
    ),
    handlers=[console_handler, file_handler]
)


logging.debug("This is a debug log")
logging.info("This is a info log")
logging.warning("This is a warning log")
logging.error("This is a error log")
logging.critical("This is a critical log")
