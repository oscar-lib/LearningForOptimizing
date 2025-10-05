import matplotlib.pyplot as plt

socket_list = [17.385, 32.151, 29.176, 24.613, 31.978, 24.236, 19.480, 30.353, 21.207, 25.365]
named_pipe_list = [29.866, 27.969, 27.269, 26.210, 29.864, 28.766, 31.492, 29.807, 24.243, 25.664, 28.745]


plt.boxplot([socket_list, named_pipe_list], label=["Unix Socket", "Named Pipe"])
plt.ylabel("Time spent in communication (%)")
plt.title("Communication Overhead Comparison")
plt.grid(axis="y")
plt.show()
