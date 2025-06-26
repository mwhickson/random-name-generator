#
# namegen.py
#
# random name generation in Python
#

import os
import random

DEBUG = True

DefaultNameDataFile: str = "./data/names.txt"

class MarkovNode:

	def __init__(self, text: str, count: int = 0):
		self.text: str = text
		self.count: int = count


class MarkovRootNode(MarkovNode):

	def __init__(self, text: str, count: int = 0):
		super().__init__(text, count)
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
	def get_name(markov_data: dict[str, MarkovRootNode], starts_with: str, minimum_length: int, maximum_length: int) -> str:
		name: str = ""
		name_length = random.randint(minimum_length, maximum_length)

		if len(starts_with) > 0 and ord(starts_with[0].lower()) in range(ord("a"), ord("z")):
			name = starts_with[0].lower()
		else:
			keys = list(markov_data.keys())
			name = random.choice(keys)

		while len(name) <= name_length:
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

		result.sort()

		return result

	@staticmethod
	def markovize_name_segments(name_segment_array: list[str]) -> dict[str, MarkovRootNode]:
		result: dict[str, MarkovRootNode] = {}

		max_segment_length = 0
		for segment in name_segment_array:
			if len(segment) > max_segment_length:
				max_segment_length = len(segment)

		if max_segment_length > 1:
			for i in range(1, max_segment_length):
				for segment in name_segment_array:
					if len(segment) > i:
						current_char = segment[i - 1].lower()
						next_char = segment[i].lower()

						if current_char not in result:
							result[current_char] = MarkovRootNode(current_char)
						else:
							result[current_char].count = result[current_char].count + 1

						if next_char not in result[current_char].exits:
							exit_node = MarkovNode(next_char)
							exit_node.count = 1
							result[current_char].exits[next_char] = exit_node
						else:
							result[current_char].exits[next_char].count = result[current_char].exits[next_char].count + 1

		if DEBUG:
			NameGenerator._dump_markov_data(result)

		return result

	@staticmethod
	def _dump_markov_data(data: dict[str, MarkovRootNode]):
		print("-----")
		for k in data:
			node = data[k]
			print(k, node.text, node.count)
			for k2 in node.exits:
				node2 = node.exits[k2]
				print("\t", node2.text, node2.count)
		print("-----")

if __name__ == "__main__":
	print("*** Random Name Generator ***")

	data = NameGenerator.get_name_data(DefaultNameDataFile)
	names = NameGenerator.get_names(data, "?", 3, 10, 10)
#	names = NameGenerator.get_names(data, "f", 3, 10, 10)

	print(names)
