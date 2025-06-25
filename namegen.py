#
# namegen.py
#
# random name generation in Python
#

import os
import random

DefaultNameDataFile: str = "./data/names.txt"


class NameGenerator:

	def __init__(self):
		pass

	@staticmethod
	def get_name_data(filename: str) -> list[str]:
		if os.path.exists(filename):
			f = open(filename, "rt")
			lines = f.read()
			f.close()
			return lines.splitlines()
		else:
			return []

	@staticmethod
	def get_name(name_segment_array: list[str], starts_with: str, minimum_length: int, maximum_length: int) -> str:
		name = ""
		name_length = random.randint(minimum_length, maximum_length)

		while len(name) <=  name_length:
			selected_segment = name_segment_array[random.randint(0, len(name_segment_array) - 1)]
			name = name + selected_segment

		if len(name) > name_length:
			name = name[:name_length].capitalize()

		return name

	@staticmethod
	def get_names(name_segment_array: list[str], starts_with: str, minimum_length: int, maximum_length: int, count: int) -> list[str]:
		result: list[str] = []

		for i in range(0, count):
			name = NameGenerator.get_name(name_segment_array, starts_with, minimum_length, maximum_length)
			result.append(name)

		return result

if __name__ == "__main__":
	print("*** Random Name Generator ***")

	data = NameGenerator.get_name_data(DefaultNameDataFile)
	names = NameGenerator.get_names(data, "?", 3, 10, 1)

	# print(data) # DEBUG:
	print(names)