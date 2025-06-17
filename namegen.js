// REF: https://healeycodes.com/generating-text-with-markov-chains

const JsonFileUrl = "data/names.json";
const NameSeparators = [ "'", "-", ];
const SynchronousFlag = false;

const GetNameData = (url) => {
	let names = [];

	// make a synchronous pull to our local data (exposed via python's http.server)
	const request = new XMLHttpRequest();
	request.open("GET", JsonFileUrl, SynchronousFlag);
	request.send(null);

	if (request.status === 200) {
		names = JSON.parse(request.responseText);
	}

	return names;
}

const GetName = (data, startsWith, minimumLength, maximumLength) => {
	const MinNameLength = minimumLength - 1;
	const MaxNameLength = maximumLength;
	const NameLength = Math.ceil(Math.random() * (MaxNameLength - MinNameLength)) + MinNameLength;

	const codePointA = "a".charCodeAt();
	const codePointZ = "z".charCodeAt();

	let markovSegments = {};

	for (let i = codePointA; i <= codePointZ; i++) {
		const c = String.fromCharCode(i);
		markovSegments[c] = {};
	}

	const startingIndex = Math.floor(Math.random() * (Object.keys(markovSegments).length)); // TODO: add startsWith support
	const startingChar = Object.keys(markovSegments)[startingIndex];

	for (let segmentLength = 2; segmentLength <= 3; segmentLength++) {
		NameSegments.filter(s => s.length >= segmentLength).forEach((s) => {
			const currentChar = s[segmentLength - 2];
			const nextChar = s[segmentLength - 1];

			if (!Object.keys(markovSegments[currentChar]).includes(nextChar)) {
				markovSegments[currentChar][nextChar] = 1;
			} else {
				markovSegments[currentChar][nextChar] = markovSegments[currentChar][nextChar] + 1;
			}

			if (!Object.keys(markovSegments[currentChar]).includes("possibilities")) {
				markovSegments[currentChar]["possibilities"] = 1;
			} else {
				markovSegments[currentChar]["possibilities"] = markovSegments[currentChar]["possibilities"] + 1;
			}
		});
	}

	let name = startingChar.toUpperCase();

	const pickNextLetter = (startingChar) => {
		let selections = [];
		const possibilities = markovSegments[startingChar].possibilities;
		let cumulativeProbability = 0;
		Object.keys(markovSegments[startingChar]).forEach((k) => {
			if (k !== "possibilities") {
				const count = markovSegments[startingChar][k];
				let probability = Math.floor(count / possibilities * 100);
				if (probability < 1) { probability = 1 }
				selections.push({
					value: k,
					low: cumulativeProbability,
					high: cumulativeProbability + probability,
				});
				cumulativeProbability += probability;
			}
		});

		const selectedValue = Math.floor(Math.random() * cumulativeProbability);
		const selected = selections.find((s) => (s.low <= selectedValue) && (s.high >= selectedValue));
		return selected ? selected.value : "";
	}

	let currentLetter = startingChar;
	for (let i = 1; i < NameLength; i++) {
		currentLetter = pickNextLetter(currentLetter);
		name += currentLetter;
	}

	return name;
}

const GetNames = (data, startsWith, minimumLength, maximumLength, count) => {
	const names = [];

	for (let i = 1; i < count; i++) {
		names.push(GetName(data, startsWith, minimumLength, maximumLength));
	}

	return names;
}

//
// main
//

const NameSegments = GetNameData(JsonFileUrl);
const names = GetNames(NameSegments, "a", 3, 9, 10);
const namesContainer = document.getElementById("names");

namesContainer.innerHTML = "<ul>" +
	names.sort().map((n) => `<li>${n}</li>`).join("")
	+ "</ul>"; 

