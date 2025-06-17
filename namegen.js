const NAME_JSON_FILE = "data/names.json";
const SYNCHRONOUS_FLAG = false;

let NameSegments = [];

// make a synchronous pull to our local data (exposed via python's http.server)
const request = new XMLHttpRequest();
request.open("GET", NAME_JSON_FILE, SYNCHRONOUS_FLAG);
request.send(null);

if (request.status === 200) {
	NameSegments = JSON.parse(request.responseText);
}

const NameSeparators = [ "'", "-", ];

// REF: https://healeycodes.com/generating-text-with-markov-chains

let markovSegments = {};

for (let i = "a".charCodeAt(); i <= "z".charCodeAt(); i++) {
	const c = String.fromCharCode(i);
	markovSegments[c] = {};
}

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

const MinNameLength = 2;
const MaxNameLength = 9;
const NameLength = Math.ceil(Math.random() * (MaxNameLength - MinNameLength)) + MinNameLength;

const startingIndex = Math.floor(Math.random() * (Object.keys(markovSegments).length));
const startingChar = Object.keys(markovSegments)[startingIndex];

let name = startingChar.toUpperCase();

const pickNextLetter = (startingChar) => {
	let selections = [];
	const possibilities = markovSegments[startingChar].possibilities;
	let cumulativeProbability = 0;
	Object.keys(markovSegments[startingChar]).forEach((k) => {
		if (k !== "possibilities") {
			const count = markovSegments[startingChar][k];
			let probability = Math.floor(count/possibilities*100);
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

document.writeln(`<h1>${name}</h1>`);
