#
# namegen.py
#
# random name generation in Python
#

import os

DefaultNameDataFile: str = "./data/names.txt"


class NameGenerator:

	def __init__(self):
		pass

	@staticmethod
	def get_name_data(filename: str) -> list[str]:
		if os.path.exists(filename):
			f = open(filename, "rt")
			lines = f.readlines()
			f.close()
			return lines
		else:
			return []

	def get_name(self, name_segment_array: list[str], starts_with: str, minimum_length: int, maximum_length: int) -> str:
		pass

	def get_names(self, name_segment_array: list[str], starts_with: str, minimum_length: int, maximum_length: int, count: int) -> list[str]:
		pass

if __name__ == "__main__":
	# n = NameGenerator()
	data = NameGenerator.get_name_data(DefaultNameDataFile)
	print(data)
