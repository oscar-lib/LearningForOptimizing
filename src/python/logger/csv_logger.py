import os
import time
import csv
import threading
import queue
from .abstract_logger import AbstractLogger


class CSVLogger(AbstractLogger):
    def __init__(self, filename: str, quiet: bool = False, flush_interval_sec: float = 10):
        AbstractLogger.__init__(self, os.path.dirname(filename), quiet)
        self.filename = filename
        self._flush_interval = flush_interval_sec

        # Threading components
        self._queue = queue.Queue()
        self._shutdown_event = threading.Event()
        self._worker_thread = threading.Thread(target=self._worker, daemon=True)

        # File and writer (used by worker thread)
        self._file = None
        self._writer = None
        self._next_flush = None

        # Start the worker thread
        self._worker_thread.start()

    def log(self, data: dict[str, float], time_step: int):
        if len(data) == 0:
            return

        # Add timestamp and time_step to data
        log_data = data.copy()
        log_data["timestamp_sec"] = time.time()
        log_data["time_step"] = time_step

        # Put data in queue for background processing
        self._queue.put(("log", log_data))

    def _worker(self):
        """Background worker thread that handles file I/O operations."""
        self._file = open(self.filename, "w")
        self._next_flush = time.time() + self._flush_interval

        try:
            while not self._shutdown_event.is_set():
                try:
                    # Wait for data with a timeout to allow checking shutdown event
                    item = self._queue.get(timeout=1.0)

                    if item[0] == "log":
                        self._write_log_data(item[1])
                    elif item[0] == "shutdown":
                        break

                    self._queue.task_done()

                except queue.Empty:
                    # Timeout occurred, check if we need to flush
                    self._check_and_flush()
                    continue

        except Exception as e:
            if not self.quiet:
                print(f"Error in CSV logger worker thread: {e}")
        finally:
            if self._file:
                self._file.flush()
                self._file.close()

    def _write_log_data(self, data: dict[str, float]):
        """Write log data to file (called by worker thread)."""
        if self._writer is None and self._file is not None:
            self._writer = csv.DictWriter(self._file, fieldnames=data.keys())
            self._writer.writeheader()

        try:
            if self._writer is not None:
                self._writer.writerow(data)
        except ValueError:
            self._rewrite_header(list(data.keys()))
            if self._writer is not None:
                self._writer.writerow(data)

        self._check_and_flush()

    def _check_and_flush(self):
        """Check if it's time to flush and do so if needed."""
        if self._file and self._next_flush is not None and time.time() >= self._next_flush:
            self._file.flush()
            self._next_flush = time.time() + self._flush_interval

    def _rewrite_header(self, headers: list[str]):
        """Rewrite header when new fields are added (called by worker thread)."""
        if self._file:
            self._file.flush()
            self._file.close()

        assert self._writer is not None
        # Keep the same order for the headers
        new_headers = list(self._writer.fieldnames) + [h for h in headers if h not in self._writer.fieldnames]

        # Manually rewrite the first line (header)
        with open(self.filename, "r") as f:
            lines = f.readlines()
        lines[0] = ",".join(new_headers) + "\n"

        self._file = open(self.filename, "w")
        self._file.writelines(lines)

        # Reinitialize the writer
        self._writer = csv.DictWriter(self._file, fieldnames=new_headers)

    def close(self):
        """Close the logger and wait for all pending writes to complete."""
        # Signal shutdown
        self._shutdown_event.set()
        self._queue.put(("shutdown", None))

        # Wait for worker thread to finish
        if self._worker_thread.is_alive():
            self._worker_thread.join(timeout=5.0)  # Wait up to 5 seconds

        # Ensure all queued items are processed
        try:
            while not self._queue.empty():
                self._queue.get_nowait()
                self._queue.task_done()
        except queue.Empty:
            pass
