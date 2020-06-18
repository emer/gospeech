package lettersound

const WordTypeUnknown = "j"
const WordTypeDelimiter = '%'
const MaxWordLength = 1024
const MaxPronounciationLength = 8192

//const MAX(a,b) = (a > b ? a: b)
//const WORDEND(word,string) = (!strcmp(MAX(word+strlen(word)-strlen(string), word), string))

// WordType returns the word type based on the word spelling.
func WordType(word string) string {
	//const tail_entry *list_ptr
	//
	//// if word end matches list, return corresponding type
	//for (list_ptr = tail_list; list_ptr- > tail; list_ptr++) {
	//	if WORDEND(word, list_ptr- > tail) {
	//		return list_ptr- >
	//		type
	//	}
	//}
	//
	//// else return unknown word type
	return WordTypeUnknown
}

// LetterToSound returns pronunciation of word based on letter-to-sound
//func letter_to_sound(word string, std::vector<char>& pronunciation) {
//char buffer[MAX_WORD_LENGTH + 3]
//	int number_of_syllables = 0
//
//	pronunciation.assign(MAX_PRONUNCIATION_LENGTH + 1, '\0')
//
//	// FORMAT WORD
//sprintf(buffer, "#%s#", word)
//
//	// CONVERT WORD TO PRONUNCIATION
//if !word_to_patphone(buffer) {
//isp_trans(buffer, &pronunciation[0])
//	// ATTEMPT TO MARK SYLL/STRESS
//number_of_syllables = syllabify(&pronunciation[0])
//	if apply_stress(&pronunciation[0], word) { // error
//pronunciation.clear()
//		return
//	}
//} else {
//strcpy(&pronunciation[0], buffer)
//}
//
//// APPEND WORD_TYPE_DELIMITER
//pronunciation[strlen(&pronunciation[0]) - 1] = WORD_TYPE_DELIMITER
//
//	// GUESS TYPE OF WORD
//if number_of_syllables != 1 {
//strcat(&pronunciation[0], word_type(word))
//} else {
//strcat(&pronunciation[0], WORD_TYPE_UNKNOWN)
//}
//
//// return resulting pronunciation
//return
//}
