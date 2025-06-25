#
# namegen.py
#
# random name generation in Python
#

import os
import random

DefaultNameDataFile: str = "./data/names.txt"

class MarkovNode:

	def __init__(self, text: str, count: int = 0):
		self.text: str = text
		self.count: int = count
		self.exits: dict[str, MarkovNode] = {}


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
	def get_name(markov_data: dict[str, MarkovNode], starts_with: str, minimum_length: int, maximum_length: int) -> str:
		name = ""
		name_length = random.randint(minimum_length, maximum_length)

		if len(starts_with) > 0 and ord(starts_with[0].lower()) in range(ord("a"), ord("z")):
			name = starts_with[0].lower()
		else:
			keys = list(markov_data.keys())
			name = random.choice(keys)

		while len(name) <=  name_length:
			exits = list(markov_data[name[-1:]].exits)
			next_character = random.choice(exits)
			name = name + next_character

		if len(name) > name_length:
			name = name[:name_length].capitalize()

		return name

	@staticmethod
	def get_names(name_segment_array: list[str], starts_with: str, minimum_length: int, maximum_length: int, count: int) -> list[str]:
		result: list[str] = []

		markov_data = NameGenerator.markovize_name_segments(name_segment_array)

		for i in range(0, count):
			name = NameGenerator.get_name(markov_data, starts_with, minimum_length, maximum_length)
			result.append(name)

		return result

	@staticmethod
	def markovize_name_segments(name_segment_array: list[str]) -> dict[str, MarkovNode]:
		result: dict[str, MarkovNode] = {}

		# for i in range(ord("a"), ord("z")):
		# 	result[chr(i)] = MarkovNode(chr(i))

		for segment in name_segment_array:
			if len(segment) > 1:
				current_char = segment[0].lower()
				next_char = segment[1].lower()

				if current_char not in result:
					result[current_char] = MarkovNode(current_char)
				else:
					result[current_char].count = result[current_char].count + 1

				if next_char not in result[current_char].exits:
					exit_node = MarkovNode(next_char)
					exit_node.count = 1
					result[current_char].exits[next_char] = exit_node
				else:
					result[current_char].exits[next_char].count = result[current_char].exits[next_char].count + 1

		return result

if __name__ == "__main__":
	print("*** Random Name Generator ***")

	data = NameGenerator.get_name_data(DefaultNameDataFile)
	names = NameGenerator.get_names(data, "?", 3, 10, 1)

	# print(data) # DEBUG:
	print(names)