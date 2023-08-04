import signal
import time
import sys

class GracefulKiller:
  kill_now = False
  def __init__(self):
    signal.signal(signal.SIGINT, self.exit_gracefully)
    signal.signal(signal.SIGTERM, self.exit_gracefully)
    self.signals_received = []

  def exit_gracefully(self, *args):
    self.signals_received.append({
      "args": args,
      "time": time.time()
    })

    if len(self.signals_received) == 1:
        # First signal received
        print("SIGTERM signal received, trying to exit gracefully in 3 seconds...")
        print(args)
        time.sleep(3)
        self.kill_now = True
    else:
        # Second or more, exit
        print("SIGTERM received again. Non-graceful exit!")
        sys.exit(1)

if __name__ == '__main__':
  killer = GracefulKiller()
  while not killer.kill_now:
    time.sleep(1)
    print("doing something in a loop ...")

  print("End of the program. I was killed gracefully :)")
