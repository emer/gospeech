package textparse

import (
	"bufio"
	"fmt"
	"log"
	"unicode"
)

// ToDo: All of this is a stub to get up and running
type TextParser struct {
	table map[string]string

	AuxStream bufio.Reader
	Escape    rune
}

func NewTextParser() *TextParser {
	tp := TextParser{}
	tp.Escape = DefaultEscapeCharacter
	tp.table = make(map[string]string)
	tp.table["emergent"] = "/c // /0 # /w /l i./*m_er_r.j_uh_n_t # // /c"
	return &tp
}

// Just does a lookup for bootstrapping - ToDo: port the code!
// ParseText returns the pregenerated phonetic version of the string argument e.g. "emergent" returns "/c // /0 # /w /l i./*m_er_r.j_uh_n_t # // /c"
func (tp *TextParser) ParseText(s string) string {
	p := tp.table[s]
	return p
}

/***************************************************************************
*  Copyright 1991, 1992, 1993, 1994, 1995, 1996, 2001, 2002               *
*    David R. Hill, Leonard Manzara, Craig Schock                         *
                                                 *
*  This program is free software: you can redistribute it and/or modify   *
*  it under the terms of the GNU General Public License as published by   *
*  the Free Software Foundation, either version 3 of the License, or      *
*  (at your option) any later version.                                    *
                                                 *
*  This program is distributed in the hope that it will be useful,        *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *
*  GNU General Public License for more details.                           *
                                                 *
*  You should have received a copy of the GNU General Public License      *
*  along with this program.  If not, see <http://www.gnu.org/licenses/>.  *
***************************************************************************/
// 2014-09
// This file was copied from Gnuspeech and modified by Marcelo Y. Matuda.

/******************************************************************************
*
*     parser_module.c
*
*     History:
*
*     July 7th, 1992          Completed.
*     December 12th, 1994     Added word begin /w and utterance
*                             boundary # markers.
*     January 5th, 1995       Fixed illegal_slash_code() so that it will
*                             recognize the new /w code when doing raw mode
*                             checking.  The # marker is a phone, so the new
*                             validPhone() function should return this as
*                             valid.  Also changed all closing of streams to
*                             use NX_FREEBUFFER instead of NX_TRUNCATEBUFFER,
*                             eliminating a potential memory leak.  The NeXT
*                             documentation is wrong, since it recommends
*                             using NX_TRUNCATEBUFFER, plus NXGetMemoryBuffer()
*                             and vm_deallocate() calls to free the internal
*                             stream buffer.
*     March 7th, 1995         Fixed bug when using medial punctuation (,;:)
*                             at the end of an utterance.
*
******************************************************************************/

/*  LOCAL DEFINES  ***********************************************************/
const UndefinedMode = -2
const NormalMode = -1
const RawMode = 0
const LetterMode = 1
const EmphasisMode = 2
const TaggingMode = 3
const SilenceMode = 4

const RawModeBegin = -1
const RawModeEnd = -2
const LetterModeBegin = -3
const LetterModeEnd = -4
const EmphasisModeBegin = -5
const EmphasisModeEnd = -6
const TaggingModeBegin = -7
const TaggingModeEnd = -8
const SilenceModeBegin = -9
const SilenceModeEnd = -10
const Deleted = -11

const Begin = 0
const End = 1

const Word = 0
const Punctuation = 1
const Pronounciation = 1

const And = "and"
const Plus = "plus"
const IsLessThan = "is less than"
const IsGreaterThan = "is greater than"
const Equals = "equals"
const Minus = "minus"
const At = "at"

const Abbreviation = 0
const Expansion = 1

const StateUndefined = -1
const StateBegin = 0
const StateWord = 1
const StateMedialPunc = 2
const StateFinalPunc = 3
const StateEnd = 4
const StateSilence = 5
const StateTagging = 6

const ChunkBoundary = "/c"
const ToneGroupBoundary = "//"
const FootBegin = "/_"
const TonicBegin = "/*"
const SecondaryStress = "/\""
const LastWord = "/l"
const TagBegin = "/t"
const WordBegin = "/w"
const UtteranceBoundary = "#"
const MedialPause = "^"
const LongMedialPause = "^ ^ ^"
const SilencePhone = "^"

const TgUndefined = "/x"
const TgStatement = "/0"
const TgExclamation = "/1"
const TgQuestion = "/2"
const TgContinuation = "/3"
const TgHalfPeriod = "/4"

const UndefinedPosition = -1

const TtsFalse = 0
const TtsTrue = 1
const TtsNo = 0
const TtsYes = 1

const SymbolLengthMax = 12

const WordLengthMax = 1024
const SilenceMax = 5.0
const SilencePhoneLength = 0.1 /*  SILENCE PHONE IS 100ms  */

const DefaultEndPunc = "."
const ModeNestMax = 100

const NonPhoneme = 0
const Phoneme = 1
const MaxPhonesPerChunk = 1500
const MaxFeetPerChunk = 100

const DefaultEscapeCharacter = 27

/*  Dictionary Ordering Definitions  */
const TtsEmpty = 0
const TtsNumberParser = 1
const TtsDictionary1 = 2
const TtsDictionary2 = 3
const TtsDictionary3 = 4
const TtsLetterToSound = 5

const TtsParserSuccess = -1
const TtsParserFailure = 0 /*  OR GREATER THAN 0 IF     */

///*  POSITION OF ERROR KNOWN  */
//
// StripPunctuation deletes unnecessary punctuation, and converts some punctuation to another form.
func (tp *TextParser) StripPunctuation(buf []rune) []rune {
	buflen := len(buf)
	mode := NormalMode
	for i := 0; i < buflen; i++ {
		switch buf[i] {
		case RawModeBegin:
			mode = RawMode
		case LetterModeBegin:
			mode = LetterMode
		case EmphasisModeBegin:
			mode = EmphasisMode
		case TaggingModeBegin:
			mode = TaggingMode
		case SilenceModeBegin:
			mode = SilenceMode
		case RawModeEnd:
			fallthrough
		case LetterModeEnd:
			fallthrough
		case EmphasisModeEnd:
			fallthrough
		case TaggingModeEnd:
			fallthrough
		case SilenceModeEnd:
			mode = NormalMode
		default:
			if mode == NormalMode || mode == EmphasisMode {
				switch buf[i] {
				case '[':
					buf[i] = '('
				case ']':
					buf[i] = ')'
				//case rune('-'):
				//	if !ConvertDash(buf, &i, buflen) &&
				//					!number_follows(buf, i, length) &&
				//					!is_isolated(buf, i, length) {
				//		buf[i] = Deleted
				//	}
				//case '+':
				//	if !part_of_number(buf, i, length) && !is_isolated(buf, i, length) {
				//		buf[i] = Deleted
				//	}
				//case '\'':
				//	if !(((i - 1) >= 0) && isalpha(buf[i-1]) && ((i + 1) < length) && isalpha(buf[i+1])) {
				//		buf[i] = Deleted
				//	}
				//case '.':
				//	delete_ellipsis(buf, &i, length)
				//case '/':
				//case '$':
				//case '%':
				//	if !part_of_number(buf, i, length) {
				//		buf[i] = Deleted
				//	}
				//case '<':
				//case '>':
				//case '&':
				//case '=':
				//case '@':
				//	if !is_isolated(buf, i, length) {
				//		buf[i] = Deleted
				//	}
				case '"':
				case '`':
				case '#':
				case '*':
				case '\\':
				case '^':
				case '_':
				case '|':
				case '~':
				case '{':
				case '}':
					buf[i] = Deleted
				default:
				}
			}
		}
	}
	return buf
}

/*  SECOND PASS  */
//stream.str("");
//mode = NormalMode;  status = PUNCTUATION;
//for (i = 0; i < length; i++) {
//switch(buffer[i]) {
//case RawModeBegin:      mode = RawMode;      stream << buffer[i]; break;
//case EmphasisModeBegin: mode = EmphasisMode; stream << buffer[i]; break;
//case TaggingModeBegin:  mode = TaggingMode;  stream << buffer[i]; break;
//case SilenceModeBegin:  mode = SilenceMode;  stream << buffer[i]; break;
//case LetterModeBegin:   mode = LetterMode;   /*  expand below  */    ; break;
//
//case RawModeEnd:
//case EmphasisModeEnd:
//case TaggingModeEnd:
//case SilenceModeEnd:    mode = NormalMode;   stream << buffer[i]; break;
//case LetterModeEnd:     mode = NormalMode;   /*  expand below  */    ; break;
//
//case Deleted:
///*  CONVERT ALL Deleted CHARACTERS TO BLANKS  */
//buffer[i] = ' ';
//stream << ' ';
//break;
//
//default:
//if ((mode == NormalMode) || (mode == EmphasisMode)) {
//switch(buffer[i]) {
//case '(':
///*  CONVERT (?) AND (!) TO BLANKS  */
//if ( ((i+2) < length) && (buffer[i+2] == ')') &&
//((buffer[i+1] == '!') || (buffer[i+1] == '?')) ) {
//buffer[i] = buffer[i+1] = buffer[i+2] = ' ';
//stream << "   ";
//i += 2;
//continue;
//}
///*  ALLOW TELEPHONE NUMBER WITH AREA CODE:  (403)274-3877  */
//if (is_telephone_number(buffer, i, length)) {
//int j;
//for (j = 0; j < 12; j++) {
//stream << buffer[i++];
//}
//status = Word;
//continue;
//}
///*  CONVERT TO COMMA IF PRECEDED BY WORD, FOLLOWED BY WORD  */
//if ((status == Word) && word_follows(buffer, i, length)) {
//buffer[i] = ' ';
//stream << ", ";
//status = Punctuation;
//} else {
//buffer[i] = ' ';
//stream << ' ';
//}
//break;
//case ')':
///*  CONVERT TO COMMA IF PRECEDED BY WORD, FOLLOWED BY WORD  */
//if ((status == Word) && word_follows(buffer, i, length)) {
//buffer[i] = ',';
//stream << ", ";
//status = Punctuation;
//} else {
//buffer[i] = ' ';
//stream << ' ';
//}
//break;
//case '&':
//stream << And;
//status = Word;
//break;
//case '+':
//if (is_isolated(buffer, i, length)) {
//stream << Plus;
//} else {
//stream << '+';
//}
//status = Word;
//break;
//case '<':
//stream << IsLessThan;
//status = Word;
//break;
//case '>':
//stream << IsGreaterThan;
//status = Word;
//break;
//case '=':
//stream << Equals;
//status = Word;
//break;
//case '-':
//if (is_isolated(buffer, i, length)) {
//stream << Minus;
//} else {
//stream << '-';
//}
//status = Word;
//break;
//case '@':
//stream << AT;
//status = Word;
//break;
//case '.':
//if (!expand_abbreviation(buffer, i, length, stream)) {
//stream << buffer[i];
//status = Punctuation;
//}
//break;
//default:
//stream << buffer[i];
//if (is_punctuation(buffer[i])) {
//status = Punctuation;
//} else if (isalnum(buffer[i])) {
//status = Word;
//}
//break;
//}
//} else if (mode == LetterMode) {
///*  EXPAND LETTER MODE CONTENTS TO PLAIN WORDS OR SINGLE LETTERS  */
//
//expand_letter_mode(buffer, &i, length, stream, &status);
//continue;
//} else { /*  ELSE PASS CHARACTERS STRAIGHT THROUGH  */
//stream << buffer[i];
//}
//break;
//}
//}
//
///*  SET STREAM LENGTH  */
//*stream_length = static_cast<long>(stream.tellp());
//}

// GetState determines the current state and next state in buffer. A word or punctuation is put into word.
// Raw mode contents are expanded and written to stream.
func (tp *TextParser) GetState() int {

	// func (tp *TextParser) GetState(buf string, i *int, len int, mode, nextMode, curState, nextState, rawModeFlag *int, word string, stream *[]rune) int {

	return 0
}

//
//long j;
//int k, state = 0, current_mode;
//int *state_buffer[2];
//
///*  PUT STATE POINTERS INTO ARRAY  */
//state_buffer[0] = current_state;
//state_buffer[1] = next_state;
//
///*  GET 2 STATES  */
//for (j = *i, current_mode = *mode; j < length; j++) {
///*  FILTER THROUGH EACH CHARACTER  */
//switch (buffer[j]) {
//case RawModeBegin:      current_mode = RawMode;      break;
//case LetterModeBegin:   current_mode = LetterMode;   break;
//case EmphasisModeBegin: current_mode = EmphasisMode; break;
//case TaggingModeBegin:  current_mode = TaggingMode;  break;
//case SilenceModeBegin:  current_mode = SilenceMode;  break;
//
//case RawModeEnd:
//case LetterModeEnd:
//case EmphasisModeEnd:
//case TaggingModeEnd:
//case SilenceModeEnd:    current_mode = NormalMode;   break;
//
//default:
//if ((current_mode == NormalMode) || (current_mode == EmphasisMode)) {
///*  SKIP WHITE  */
//if (buffer[j] == ' ') {
//break;
//}
//
///*  PUNCTUATION  */
//if (is_punctuation(buffer[j])) {
//if ((buffer[j] == '.') && ((j+1) < length) && isdigit(buffer[j+1])) {
//;  /*  DO NOTHING, HANDLE AS WORD BELOW  */
//} else {
///*  SET STATE ACCORDING TO PUNCUATION TYPE  */
//switch (buffer[j]) {
//case '.':
//case '!':
//case '?':  *(state_buffer[state]) = StateFinalPunc;  break;
//case ';':
//case ':':
//case ',':  *(state_buffer[state]) = StateMedialPunc;  break;
//}
//
///*  PUT PUNCTUATION INTO WORD BUFFER, SET OUTSIDE COUNTER, IN CURRENT STATE  */
//if (state == 0) {
//word[0] = buffer[j];
//word[1] = '\0';
//*i = j;
///*  SET OUTSIDE MODE  */
//*mode = current_mode;
//} else { /*  SET NEXT MODE IF SECOND STATE  */
//*next_mode = current_mode;
//}
//
///*  INCREMENT STATE  */
//state++;
//break;
//}
//}
//
///*  WORD  */
//if (state == 0) {
///*  PUT WORD INTO BUFFER  */
//k = 0;
//do {
//word[k++] = buffer[j++];
//} while ((j < length) && (buffer[j] != ' ') &&
//!is_mode(buffer[j]) && (k < WordLengthMax));
//word[k] = '\0'; j--;
//
///*  BACK UP IF WORD ENDS WITH PUNCTUATION  */
//while (k >= 1) {
//if (is_punctuation(word[k-1])) {
//word[--k] = '\0';
//j--;
//} else {
//break;
//}
//}
//
///*  SET OUTSIDE COUNTER  */
//*i = j;
//
///*  SET OUTSIDE MODE  */
//*mode = current_mode;
//} else {
///*  SET NEXT MODE IF SECOND STATE  */
//*next_mode = current_mode;
//}
//
///*  SET STATE TO Word, INCREMENT STATE  */
//*(state_buffer[state++]) = StateWord;
//break;
//} else if ((current_mode == SilenceMode) && (state == 0)) {
///*  PUT SILENCE LENGTH INTO WORD BUFFER IN CURRENT STATE ONLY  */
//k = 0;
//do {
//word[k++] = buffer[j++];
//} while ((j < length) && !is_mode(buffer[j]) && (k < Word_LENGTH_MAX));
//word[k] = '\0';  j--;
//
///*  SET OUTSIDE COUNTER  */
//*i = j;
//
///*  SET OUTSIDE MODE  */
//*mode = current_mode;
//
///*  SET STATE TO SILENCE, INCREMENT STATE  */
//*(state_buffer[state++]) = StateSilence;
//} else if ((current_mode == TaggingMode) && (state == 0)) {
///*  PUT TAG INTO WORD BUFFER IN CURRENT STATE ONLY  */
//k = 0;
//do {
//word[k++] = buffer[j++];
//} while ((j < length) && !is_mode(buffer[j]) && (k < Word_LENGTH_MAX));
//word[k] = '\0';  j--;
//
///*  SET OUTSIDE COUNTER  */
//*i = j;
//
///*  SET OUTSIDE MODE  */
//*mode = current_mode;
//
///*  SET STATE TO TAGGING, INCREMENT STATE  */
//*(state_buffer[state++]) = StateTagging;
//} else if ((current_mode == RawMode) && (state == 0)) {
///*  EXPAND RAW MODE IN CURRENT STATE ONLY  */
//if (expand_raw_mode(buffer, &j, length, stream) != TtsParserSuccess) {
//return(TtsParserFailure);
//}
//
///*  SET RawMode FLAG  */
//*raw_mode_flag = TtsTrue;
//
///*  SET OUTSIDE COUNTER  */
//*i = j;
//}
//break;
//}
//
///*  ONLY NEED TWO STATES  */
//if (state >= 2) {
//return TtsParserSuccess;
//}
//}
//
///*  IF HERE, THEN END OF INPUT BUFFER, INDICATE END STATE  */
//if (state == 0) {
///*  SET STATES  */
//*current_state = StateEnd;
//*next_state = StateUndefined;
///*  BLANK OUT WORD BUFFER  */
//word[0] = '\0';
///*  SET OUTSIDE COUNTER  */
//*i = j;
///*  SET OUTSIDE MODE  */
//*mode = current_mode;
//} else {
//*next_state = StateEnd;
//}
//
///*  RETURN SUCCESS  */
//return TtsParserSuccess;
//}
//
//// SetToneGroup sets the tone group marker according to the punctuation passed in as "word".
//// The marker is inserted in the
//stream at position "tg_pos".
//set_tone_group(std::stringstream& stream, long tg_pos, const char* word) int {
///*  return TtsParserFailure, iMMEDIATELY IF tg_pos NOT LEGAL  */
//if (tg_pos == UndefinedPosition) {
//return TtsParserFailure;
//}
//
///*  GET CURRENT POSITION IN STREAM  */
//long current_pos = static_cast<long>(stream.tellp());
//
///*  SEEK TO TONE GROUP MARKER POSITION  */
//stream.seekp(tg_pos);
//
///*  WRITE APPROPRIATE TONE GROUP TYPE  */
//switch (word[0]) {
//case '.':
//stream << TgStatement;
//break;
//case '!':
//stream << TgExclamation;
//break;
//case '?':
//stream << TgQuestion;
//break;
//case ',':
//stream << TgContinuation;
//break;
//case ';':
//stream << TgHalfPeriod;
//break;
//case ':':
//stream << TgContinuation;
//break;
//default:
//return TtsParserFailure;
//}
//
///*  SEEK TO ORIGINAL POSITION ON STREAM  */
//stream.seekp(current_pos);
//
///*  RETURN SUCCESS */
//return TtsParserSuccess;
//}
//
//// ConvertSilence converts numeric quantity in "buffer" to appropriate number of silence phones,
//// which are written onto the end of stream.  Rounding is performed.  Returns actual length of silence.
//func (tp *TextParser) ConvertSilence(const char* buffer, std::stringstream& stream) float64 {
///*  CONVERT BUFFER TO DOUBLE  */
//double silence_length = strtod(buffer, NULL);
//
///*  LIMIT SILENCE LENGTH TO MAXIMUM  */
//silence_length = (silence_length > SilenceMax) ? SilenceMax : silence_length;
//
///*  FIND EQUIVALENT NUMBER OF SILENCE PHONES, PERFORMING ROUNDING  */
//int number_silence_phones = (int) rint(silence_length / SilencePhoneLength);
//
///*  PUT IN UTTERANCE BOUNDARY MARKER  */
//stream << UtteranceBoundary << ' ';
//
///*  WRITE OUT SILENCE PHONES TO STREAMS  */
//for (int j = 0; j < number_silence_phones; j++) {
//stream << SilencePhone << ' ';
//}
//
///*  RETURN ACTUAL LENGTH OF SILENCE  */
//return static_cast<float>(number_silence_phones * SilencePhoneLength);
//}
//
//// AnotherWordFollows returns 1 if another word follows in buffer, after position i.  Else, 0 is returned
//func (tp *TextParser) AnotherWordFollows(const char* buffer, long i, long length, int mode) int {
//
//for (long j = i+1; j < length; j++) {
///*  FILTER THROUGH EACH CHARACTER  */
//switch(buffer[j]) {
//case RawModeBegin:      mode = RawMode;      break;
//case LetterModeBegin:   mode = LetterMode;   break;
//case EmphasisModeBegin: mode = EmphasisMode; break;
//case TaggingModeBegin:  mode = TaggingMode;  break;
//case SilenceModeBegin:  mode = SilenceMode;  break;
//
//case RawModeEnd:
//case LetterModeEnd:
//case EmphasisModeEnd:
//case TaggingModeEnd:
//case SilenceModeEnd:    mode = NormalMode;   break;
//
//default:
//if ((mode == NormalMode) || (mode == EmphasisMode)) {
///*  WORD HAS BEEN FOUND  */
//if (!is_punctuation(buffer[j])) {
//return 1;
//}
//}
//break;
//}
//}
//
///*  IF HERE, THEN NO WORD FOLLOWS  */
//return 0;
//}
//
//// ShiftSilence  looks past punctuation to see if some silence occurs before the next word
//// (or raw mode contents), and shifts the silence to the current point on the stream.  The
//// the numeric quantity is converted to equivalent silence phones, and a 1 is returned.  0 is returned otherwise.
//func (tp *TextParser) ShiftSilence(const char* buffer, long i, long length, int mode, std::stringstream& stream) int {
//char word[WORD_LENGTH_MAX + 1];
//
//for (long j = i + 1; j < length; j++) {
///*  FILTER THROUGH EACH CHARACTER  */
//switch (buffer[j]) {
//case RawModeBegin:      mode = RawMode;      break;
//case LetterModeBegin:   mode = LetterMode;   break;
//case EmphasisModeBegin: mode = EmphasisMode; break;
//case TaggingModeBegin:  mode = TaggingMode;  break;
//case SilenceModeBegin:  mode = SilenceMode;  break;
//
//case RawModeEnd:
//case LetterModeEnd:
//case EmphasisModeEnd:
//case TaggingModeEnd:
//case SilenceModeEnd:    mode = NormalMode;   break;
//
//default:
//if ((mode == NormalMode) || (mode == EmphasisMode)) {
///*  SKIP WHITE SPACE  */
//if (buffer[j] == ' ') {
//continue;
//}
///*  WORD HERE, SO RETURN WITHOUT SHIFTING  */
//if (!is_punctuation(buffer[j])) {
//return 0;
//}
//} else if (mode == RawMode) {
///*  ASSUME RAW MODE CONTAINS WORD OF SOME SORT  */
//return 0;
//} else if (mode == SilenceMode) {
///*  COLLECT SILENCE DIGITS INTO WORD BUFFER  */
//int k = 0;
//do {
//word[k++] = buffer[j++];
//} while ((j < length) && !is_mode(buffer[j]) && (k < Word_LENGTH_MAX));
//word[k] = '\0';
///*  CONVERT WORD TO SILENCE PHONES, APPENDING TO STREAM  */
//convert_silence(word, stream);
///*  RETURN, INDICATING SILENCE SHIFTED BACKWARDS  */
//return 1;
//}
//break;
//}
//}
//
///*  IF HERE, THEN SILENCE NOT SHIFTED  */
//return 0;
//}
//
//// InsertTag inserts the tag contained in word onto the stream at the insert point.
//func (tp *TextParser) InsertTag(std::stringstream& stream, long insert_point, const char* word)
//{
///*  return TtsParserFailure, iMMEDIATELY IF NO INSERT POINT  */
//if (insert_point == UndefinedPosition) {
//return;
//}
//
///*  FIND POSITION OF END OF STREAM  */
//long end_point = static_cast<long>(stream.tellp());
//
///*  CALCULATE HOW MANY CHARACTERS TO SHIFT  */
//long length = end_point - insert_point;
//
///*  IF LENGTH IS 0, THEN SIMPLY APPEND TAG TO STREAM  */
//if (length == 0) {
//stream << TagBegin << ' ' << word;
//} else {
///*  ELSE, SAVE STREAM AFTER INSERT POINT  */
//std::string temp(length, '\0');
//stream.seekg(insert_point);
//for (long j = 0; j < length; j++) {
//char c;
//if (!stream.get(c)) {
//THROW_EXCEPTION(GS::EndOfBufferException, "Could not get a character from the stream.");
//}
//temp[j] = c;
//}
//
///*  INSERT TAG; ADD TEMPORARY MATERIAL  */
//stream.seekp(insert_point);
//stream << TagBegin << ' ' << word << ' ' << temp;
//}
//}
//
//
//// ExpandRawMode writes raw mode contents to stream, checking phones and marker
//func (tp *TextParser) ExpandRawMode(const char *buffer, long* j, long length, std::stringstream& stream) int {
//int k, super_raw_mode = TtsFalse, delimiter = TtsFalse, blank = TtsTrue;
//char token[SymbolLengthMax+1];
//
///*  EXPAND AND CHECK RAW MODE CONTENTS TILL END OF RAW MODE  */
//token[k = 0] = '\0';
//for ( ; (*j < length) && (buffer[*j] != RawModeEnd); (*j)++) {
//stream << buffer[*j];
///*  CHECK IF ENTERING OR EXITING SUPER RAW MODE  */
//if (buffer[*j] == '%') {
//if (!super_raw_mode) {
//if (illegal_token(token)) {
//return TtsParserFailure;
//}
//super_raw_mode = TtsTrue;
//token[k = 0] = '\0';
//continue;
//} else {
//super_raw_mode = TtsFalse;
//token[k = 0] = '\0';
//delimiter = blank = TtsFalse;
//continue;
//}
//}
///*  EXAMINE SLASH CODES, DELIMITERS, AND PHONES IN REGULAR RAW MODE  */
//if (!super_raw_mode) {
//switch (buffer[*j]) {
//case '/':
///*  SLASH CODE  */
///*  EVALUATE PENDING TOKEN  */
//if (illegal_token(token)) {
//return(TtsParserFailure);
//}
///*  PUT SLASH CODE INTO TOKEN BUFFER  */
//token[0] = '/';
//if ((++(*j) < length) && (buffer[*j] != RawModeEnd)) {
//stream << buffer[*j];
//token[1] = buffer[*j];
//token[2] = '\0';
///*  CHECK LEGALITY OF SLASH CODE  */
//if (illegal_slash_code(token)) {
//return TtsParserFailure;
//}
///*  CHECK ANY TAG AND TAG NUMBER  */
//if (!strcmp(token,TagBegin)) {
//if (expand_tag_number(buffer, j, length, stream) == TtsParserFailure) {
//return TtsParserFailure;
//}
//}
///*  RESET FLAGS  */
//token[k = 0] = '\0';
//delimiter = blank = TtsFalse;
//} else {
//return TtsParserFailure;
//}
//break;
//case '_':
//case '.':
///*  SYLLABLE DELIMITERS  */
///*  DON'T ALLOW REPEATED DELIMITERS, OR DELIMITERS AFTER BLANK  */
//if (delimiter || blank) {
//return TtsParserFailure;
//}
//delimiter++;
//blank = TtsFalse;
///*  EVALUATE PENDING TOKEN  */
//if (illegal_token(token)) {
//return TtsParserFailure;
//}
///*  RESET FLAGS  */
//token[k = 0] = '\0';
//break;
//case ' ':
///*  WORD DELIMITER  */
///*  DON'T ALLOW SYLLABLE DELIMITER BEFORE BLANK  */
//if (delimiter) {
//return TtsParserFailure;
//}
///*  SET FLAGS  */
//blank++;
//delimiter = TtsFalse;
///*  EVALUATE PENDING TOKEN  */
//if (illegal_token(token)) {
//return TtsParserFailure;
//}
///*  RESET FLAGS  */
//token[k = 0] = '\0';
//break;
//default:
///*  PHONE SYMBOL  */
///*  RESET FLAGS  */
//delimiter = blank = TtsFalse;
///*  ACCUMULATE PHONE SYMBOL IN TOKEN BUFFER  */
//token[k++] = buffer[*j];
//if (k <= SymbolLengthMax) {
//token[k] = '\0';
//} else {
//return TtsParserFailure;
//}
//break;
//}
//}
//}
//
///*  CHECK ANY REMAINING TOKENS  */
//if (illegal_token(token)) {
//return TtsParserFailure;
//}
///*  CANNOT END WITH A DELIMITER  */
//if (delimiter) {
//return TtsParserFailure;
//}
//
///*  PAD WITH SPACE, RESET EXTERNAL COUNTER  */
//stream << ' ';
//(*j)--;
//
///*  RETURN SUCCESS  */
//return TtsParserSuccess;
//}
//
//// IllegalToken returns 1 if token is not a valid DEGAS phone, otherwise 0.
//func (tp *TextParser) IllegalToken(token string) int {
//if len(token) == 0 {
//return 0;
//}
//
///*  IF PHONE A VALID DEGAS PHONE, RETURN 0;  1 OTHERWISE  */
//if (1 /*validPhone(token)*/) { //TODO: implement
//return 0;
//} /*else {
//	return 1;
//}*/
//}
//
//// IllegalSlashCode returns 1 if code is illegal, 0 otherwise.
//IllegalSlashCode(code string) int {
//
//int i = 0;
//const char* legal_code[] = {
//ChunkBoundary,ToneGroupBoundary,FootBegin,
//TonicBegin,SecondaryStress,LastWord,TagBegin,
//WordBegin,TgStatement,TgExclamation,TgQuestion,
//TgContinuation,TgHalfPeriod,NULL
//};
//
///*  COMPARE CODE WITH LEGAL CODES, RETURN 0 IMMEDIATELY IF A MATCH  */
//while (legal_code[i] != NULL) {
//if (!strcmp(legal_code[i++], code)) {
//return 0;
//}
//}
//
///*  IF HERE, THEN NO MATCH;  RETURN 1, INDICATING ILLEGAL CODE  */
//return 1;
//}
//
//// ExpandTagNumber expand tag number in buffer at position j and write to stream.
//// Perform error checking, returning error code if format of tag number is illegal.
//func (tp *TextParser) ExpandTagNumber(const char* buffer, long* j, long length, std::stringstream& stream) int {
//{
///*  SKIP WHITE  */
//while ((((*j)+1) < length) && (buffer[(*j)+1] == ' ')) {
//(*j)++;
//stream << buffer[*j];
//}
//
///*  CHECK FORMAT OF TAG NUMBER  */
//int sign = 0;
//while ((((*j)+1) < length) && (buffer[(*j)+1] != ' ') &&
//(buffer[(*j)+1] != RawModeEnd) && (buffer[(*j)+1] != '%')) {
//stream << buffer[++(*j)];
//if ((buffer[*j] == '-') || (buffer[*j] == '+')) {
//if (sign) {
//return TtsParserFailure;
//}
//sign++;
//} else if (!isdigit(buffer[*j])) {
//return TtsParserFailure;
//}
//}
//
///*  RETURN SUCCESS  */
//return TtsParserSuccess;
//}
//
//// IsMode Returns 1 if character is a mode marker, otherwise 0.
//func IsMode(rune c) bool {
//if c >= SilenceModeEnd && c <= RawModeBegin {
//return true
//} else {
//return false
//}
//
//// IsIsolated returns 1 if character at position i is isolated, i.e. is surrounded by space or mode marker.  Returns 0 otherwise.
//func IsIsolated(char *buffer, int i, int len) bool {
//if ( ((i == 0) || (((i-1) >= 0) && (is_mode(buffer[i-1]) || (buffer[i-1] == ' ')))) &&
//((i == (len-1)) || (((i+1) < len) && (is_mode(buffer[i+1]) || (buffer[i+1] == ' ')))))
//return true
//} else {
//return false
//}
//
///******************************************************************************
// *
// *       function:       part_of_number
// *
// *       purpose:        Returns 1 if character at position i is part of
//a number (including mixtures with non-numeric
//characters).  Returns 0 otherwise.
// *
// ******************************************************************************/
//int
//part_of_number(char *buffer, int i, int len)
//{
//while( (--i >= 0) && (buffer[i] != ' ') && (buffer[i] != Deleted) && (!is_mode(buffer[i])) )
//if (isdigit(buffer[i]))
//return true
//
//while( (++i < len) && (buffer[i] != ' ') && (buffer[i] != Deleted) && (!is_mode(buffer[i])) )
//if (isdigit(buffer[i]))
//return true
//
//return false
//}
//
//// NumberFollows returns a 1 if at least one digit follows the character at position i, up to white space or mode marker.
//// Returns 0 otherwise.
//func NumberFollows(char *buffer, int i, int len) bool {
//{
//while( (++i < len) && (buffer[i] != ' ') &&
//(buffer[i] != Deleted) && (!is_mode(buffer[i])) )
//if (isdigit(buffer[i]))
//return true
//
//return false
//}
//
//// DeleteEllipsis deletes three dots in a row (disregarding whitespace).  If four dots,
//// then the last three are deleted.
//func DeleteEllipsis(char *buffer, int *i, int length) {
///*  SET POSITION OF FIRST DOT  */
//int pos1 = *i, pos2, pos3;
//
///*  IGNORE ANY WHITE SPACE  */
//while (((*i+1) < length) && (buffer[*i+1] == ' '))
//(*i)++;
///*  CHECK FOR 2ND DOT  */
//if (((*i+1) < length) && (buffer[*i+1] == '.')) {
//pos2 = ++(*i);
///*  IGNORE ANY WHITE SPACE  */
//while (((*i+1) < length) && (buffer[*i+1] == ' '))
//(*i)++;
///*  CHECK FOR 3RD DOT  */
//if (((*i+1) < length) && (buffer[*i+1] == '.')) {
//pos3 = ++(*i);
///*  IGNORE ANY WHITE SPACE  */
//while (((*i+1) < length) && (buffer[*i+1] == ' '))
//(*i)++;
///*  CHECK FOR 4TH DOT  */
//if (((*i+1) < length) && (buffer[*i+1] == '.'))
//buffer[pos2] = buffer[pos3] = buffer[++(*i)] = Deleted;
//else
//buffer[pos1] = buffer[pos2] = buffer[pos3] = Deleted;
//}
//}
//}
//
//
//// ConvertDash converts "--" to ", ", and "---" to ",  ". Returns 1 if this is done, 0 otherwise.
//func ConvertDash (char *buffer, int *i, int length) bool {
///*  SET POSITION OF INITIAL DASH  */
//int pos1 = *i;
//
///*  CHECK FOR 2ND DASH  */
//if (((*i+1) < length) && (buffer[*i+1] == '-')) {
//buffer[pos1] = ',';
//buffer[++(*i)] = Deleted;
///*  CHECK FOR 3RD DASH  */
//if (((*i+1) < length) && (buffer[*i+1] == '-'))
//buffer[++(*i)] = Deleted;
//return true
//}
//
///*  RETURN ZERO IF NOT CONVERTED  */
//return false
//}
//
//// IsTelephoneNumber returns true if string at position i in buffer is of the
//// form:  (ddd)ddd-dddd where each d is a digit.
//func IsTelephoneNumber(char *buffer, int i, int length) bool {
//	// ToDo: copy some go code
//	return false
//}
//
//// IsPunctuation
//func IsPunctuation(rune c) bool {
//if c == '.' || c == ',' || c == ';' || c == ':' || c == '?' || c == '!' {
//return true
//}
//return false
//}
//
//// WordFollows returns a true if a word or speakable symbol (letter mode)  follows the position i in buffer.
//// Raw, tagging, and silence mode contents are ignored.  Returns false if any punctuation (except . as part of number) follows.
//func WordFollows(const char* buffer, int i, int length) bool {
//int mode = NormalMode;
//
//for (int j = i + 1; j < length; j++) {
//switch(buffer[j]) {
//case RawModeBegin:      mode = RawMode;      break;
//case LetterModeBegin:   mode = LetterMode;   break;
//case EmphasisModeBegin: mode = EmphasisMode; break;
//case TaggingModeBegin:  mode = TaggingMode;  break;
//case SilenceModeBegin:  mode = SilenceMode;  break;
//case RawModeEnd:
//case LetterModeEnd:
//case EmphasisModeEnd:
//case TaggingModeEnd:
//case SilenceModeEnd:    mode = NormalMode;   break;
//default:
//switch(mode) {
//case NormalMode:
//case EmphasisMode:
///*  IGNORE WHITE SPACE  */
//if ((buffer[j] == ' ') || (buffer[j] == Deleted)) {
//continue;
//} else if (is_punctuation(buffer[j])) {
///*  PUNCTUATION MEANS NO WORD FOLLOWS (UNLESS PERIOD PART OF NUMBER)  */
//
//if ((buffer[j] == '.') && ((j+1) < length) && isdigit(buffer[j+1])) {
//return 1;
//} else {
//return 0;
//}
//} else { /*  ELSE, SOME WORD FOLLOWS  */
//return 1;
//}
//case LetterMode:
///*  IF LETTER MODE CONTAINS ANY SYMBOLS, THEN RETURN 1  */
//return 1;
//case RawMode:
//case SilenceMode:
//case TaggingMode:
///*  IGNORE CONTENTS OF RAW, SILENCE, AND TAGGING MODE  */
//continue;
//}
//}
//}
//
///*  IF HERE, THEN A FOLLOWING WORD NOT FOUND  */
//return 0;
//}
//
///******************************************************************************
// *
// *       function:       expand_abbreviation
// *
// *       purpose:        Expands listed abbreviations.  Two lists are used (see
//abbreviations.h):  one list expands unconditionally,
//the other only if the abbreviation is followed by a
//number.  The abbreviation p. is expanded to page.
//Single alphabetic characters have periods deleted, but
//no expansion is made.  They are also capitalized.
//Returns 1 if expansion made (i.e. period is deleted),
//0 otherwise.
// *
// ******************************************************************************/
//int
//expand_abbreviation(char* buffer, int i, int length, std::stringstream& stream)
//{
//int j, k, word_length = 0;
//char word[5];
//
///*  DELETE PERIOD AFTER SINGLE CHARACTER (EXCEPT p.)  */
//if ( ((i-1) == 0) ||  ( ((i-2) >= 0) &&
//( (buffer[i-2] == ' ') || (buffer[i-2] == '.') || (is_mode(buffer[i-2])) )
//) ) {
//if (isalpha(buffer[i-1])) {
//if ((buffer[i-1] == 'p') && (((i-1) == 0) || (((i-2) >= 0) && (buffer[i-2] != '.')) ) ) {
///*  EXPAND p. TO page  */
//stream.seekp(-1, std::ios_base::cur);
//stream << "page ";
//} else {
///*  ELSE, CAPITALIZE CHARACTER IF NECESSARY, BLANK OUT PERIOD  */
//stream.seekp(-1, std::ios_base::cur);
//if (islower(buffer[i-1])) {
//buffer[i-1] = toupper(buffer[i-1]);
//}
//stream << buffer[i-1] << ' ';
//}
///*  INDICATE ABBREVIATION EXPANDED  */
//return 1;
//}
//}
//
///*  GET LENGTH OF PRECEDING ISOLATED STRING, UP TO 4 CHARACTERS  */
//for (j = 2; j <= 4; j++) {
//if (((i-j) == 0) ||
//(((i-(j+1)) >= 0) && ((buffer[i-(j+1)] == ' ') || (is_mode(buffer[i-(j+1)]))) ) ) {
//if (isalpha(buffer[i-j]) && isalpha(buffer[i-j+1])) {
//word_length = j;
//break;
//}
//}
//}
//
///*  IS ABBREVIATION ONLY IF WORD LENGTH IS 2, 3, OR 4 CHARACTERS  */
//if ((word_length >= 2) && (word_length <= 4)) {
///*  GET ABBREVIATION  */
//for (k = 0, j = i - word_length; k < word_length; k++) {
//word[k] = buffer[j++];
//}
//word[k] = '\0';
//
///*  EXPAND THESE ABBREVIATIONS ONLY IF FOLLOWED BY NUMBER  */
//for (j = 0; abbr_with_number[j][Abbreviation] != NULL; j++) {
//if (!strcmp(abbr_with_number[j][Abbreviation],word)) {
///*  IGNORE WHITE SPACE  */
//while (((i+1) < length) && ((buffer[i+1] == ' ') || (buffer[i+1] == Deleted))) {
//i++;
//}
///*  EXPAND ONLY IF NUMBER FOLLOWS  */
//if (number_follows(buffer, i, length)) {
//stream.seekp(-word_length, std::ios_base::cur);
//stream << abbr_with_number[j][Expansion] << ' ';
//return 1;
//}
//}
//}
//
///*  EXPAND THESE ABBREVIATIONS UNCONDITIONALLY  */
//for (j = 0; abbreviation[j][Abbreviation] != NULL; j++) {
//if (!strcmp(abbreviation[j][Abbreviation],word)) {
//stream.seekp(-word_length, std::ios_base::cur);
//stream << abbreviation[j][Expansion] << ' ';
//return 1;
//}
//}
//}
//
///*  IF HERE, THEN NO EXPANSION MADE  */
//return 0;
//}
//
///******************************************************************************
// *
// *       function:       expand_letter_mode
// *
// *       purpose:        Expands contents of letter mode string to word or
//words.  A comma is added after each expansion, except
//the last letter when it is followed by punctuation.
// *
// ******************************************************************************/
//void
//expand_letter_mode(const char* buffer, int* i, int length, std::stringstream& stream, int* status)
//{
//for ( ; ((*i) < length) && (buffer[*i] != LetterModeEnd); (*i)++) {
///*  CONVERT LETTER TO WORD OR WORDS  */
//switch (buffer[*i]) {
//case ' ': stream << "blank";                break;
//case '!': stream << "exclamation point";    break;
//case '"': stream << "double quote";         break;
//case '#': stream << "number sign";          break;
//case '$': stream << "dollar";               break;
//case '%': stream << "percent";              break;
//case '&': stream << "ampersand";            break;
//case '\'':stream << "single quote";         break;
//case '(': stream << "open parenthesis";     break;
//case ')': stream << "close parenthesis";    break;
//case '*': stream << "asterisk";             break;
//case '+': stream << "plus sign";            break;
//case ',': stream << "comma";                break;
//case '-': stream << "hyphen";               break;
//case '.': stream << "period";               break;
//case '/': stream << "slash";                break;
//case '0': stream << "zero";                 break;
//case '1': stream << "one";                  break;
//case '2': stream << "two";                  break;
//case '3': stream << "three";                break;
//case '4': stream << "four";                 break;
//case '5': stream << "five";                 break;
//case '6': stream << "six";                  break;
//case '7': stream << "seven";                break;
//case '8': stream << "eight";                break;
//case '9': stream << "nine";                 break;
//case ':': stream << "colon";                break;
//case ';': stream << "semicolon";            break;
//case '<': stream << "open angle bracket";   break;
//case '=': stream << "equal sign";           break;
//case '>': stream << "close angle bracket";  break;
//case '?': stream << "question mark";        break;
//case '@': stream << "at sign";              break;
//case 'A':
//case 'a': stream << 'A';                    break;
//case 'B':
//case 'b': stream << 'B';                    break;
//case 'C':
//case 'c': stream << 'C';                    break;
//case 'D':
//case 'd': stream << 'D';                    break;
//case 'E':
//case 'e': stream << 'E';                    break;
//case 'F':
//case 'f': stream << 'F';                    break;
//case 'G':
//case 'g': stream << 'G';                    break;
//case 'H':
//case 'h': stream << 'H';                    break;
//case 'I':
//case 'i': stream << 'I';                    break;
//case 'J':
//case 'j': stream << 'J';                    break;
//case 'K':
//case 'k': stream << 'K';                    break;
//case 'L':
//case 'l': stream << 'L';                    break;
//case 'M':
//case 'm': stream << 'M';                    break;
//case 'N':
//case 'n': stream << 'N';                    break;
//case 'O':
//case 'o': stream << 'O';                    break;
//case 'P':
//case 'p': stream << 'P';                    break;
//case 'Q':
//case 'q': stream << 'Q';                    break;
//case 'R':
//case 'r': stream << 'R';                    break;
//case 'S':
//case 's': stream << 'S';                    break;
//case 'T':
//case 't': stream << 'T';                    break;
//case 'U':
//case 'u': stream << 'U';                    break;
//case 'V':
//case 'v': stream << 'V';                    break;
//case 'W':
//case 'w': stream << 'W';                    break;
//case 'X':
//case 'x': stream << 'X';                    break;
//case 'Y':
//case 'y': stream << 'Y';                    break;
//case 'Z':
//case 'z': stream << 'Z';                    break;
//case '[': stream << "open square bracket";  break;
//case '\\':stream << "back slash";           break;
//case ']': stream << "close square bracket"; break;
//case '^': stream << "caret";                break;
//case '_': stream << "under score";          break;
//case '`': stream << "grave accent";         break;
//case '{': stream << "open brace";           break;
//case '|': stream << "vertical bar";         break;
//case '}': stream << "close brace";          break;
//case '~': stream << "tilde";                break;
//default:  stream << "unknown";              break;
//}
///*  APPEND COMMA, UNLESS PUNCTUATION FOLLOWS LAST LETTER  */
//if ( (((*i)+1) < length) &&
//(buffer[(*i)+1] == LetterModeEnd) &&
//!word_follows(buffer, (*i), length)) {
//stream << ' ';
//*status = WORD;
//} else {
//stream << ", ";
//*status = Punctuation;
//}
//}
///*  BE SURE TO SET INDEX BACK ONE, SO CALLING ROUTINE NOT FOULED UP  */
//(*i)--;
//}
//
///******************************************************************************
// *
// *       function:       is_all_upper_case
// *
// *       purpose:        Returns 1 if all letters of the word are upper case,
//0 otherwise.
// *
// ******************************************************************************/
//int
//is_all_upper_case(const char* word)
//{
//while (*word) {
//if (!isupper(*word)) {
//return 0;
//}
//word++;
//}
//
//return 1;
//}
//
///******************************************************************************
// *
// *       function:       to_lower_case
// *
// *       purpose:        Converts any upper case letter in word to lower case.
// *
// ******************************************************************************/
//char*
//to_lower_case(char* word)
//{
//char *ptr = word;
//
//while (*ptr) {
//if (isupper(*ptr))
//*ptr = tolower(*ptr);
//ptr++;
//}
//
//return(word);
//}
//
///******************************************************************************
// *
// *       function:       is_special_acronym
// *
// *       purpose:        Returns a pointer to the pronunciation of a special
//acronym if it is defined in the list.  Otherwise,
//NULL is returned.
// *
// ******************************************************************************/
//const char*
//is_special_acronym(const char* word)
//{
//const char* acronym;
//
///*  LOOP THROUGH LIST UNTIL MATCH FOUND, RETURN PRONUNCIATION  */
//for (int i = 0; (acronym = special_acronym[i][WORD]); i++) {
//if (!strcmp(word, acronym)) {
//return special_acronym[i][Pronounciation];
//}
//}
//
///*  IF HERE, NO SPECIAL ACRONYM FOUND, RETURN NULL  */
//return nullptr;
//}
//
///******************************************************************************
// *
// *       function:       contains_primary_stress
// *
// *       purpose:        Returns 1 if the pronunciation contains ' (and ` for
//backwards compatibility).  Otherwise 0 is returned.
// *
// ******************************************************************************/
//int
//contains_primary_stress(const char *pronunciation)
//{
//for ( ; *pronunciation && (*pronunciation != '%'); pronunciation++)
//if ((*pronunciation == '\'') || (*pronunciation == '`'))
//return(TtsYes);
//
//return(TtsNo);
//}
//
///******************************************************************************
// *
// *       function:       converted_stress
// *
// *       purpose:        Returns 1 if the first " is converted to a ',
//otherwise 0 is returned.
// *
// ******************************************************************************/
//int
//converted_stress(char *pronunciation)
//{
///*  LOOP THRU PRONUNCIATION UNTIL " FOUND, REPLACE WITH '  */
//for ( ; *pronunciation && (*pronunciation != '%'); pronunciation++)
//if (*pronunciation == '"') {
//*pronunciation = '\'';
//return(TtsYes);
//}
//
///*  IF HERE, NO " FOUND  */
//return(TtsNo);
//}
//
///******************************************************************************
// *
// *       function:       is_possessive
// *
// *       purpose:        Returns 1 if 's is found at end of word, and removes
//the 's ending from the word.  Otherwise, 0 is returned.
// *
// ******************************************************************************/
//int
//is_possessive(char* word)
//{
///*  LOOP UNTIL 's FOUND, REPLACE ' WITH NULL  */
//for ( ; *word; word++) {
//if ((*word == '\'') && *(word+1) && (*(word+1) == 's') && (*(word+2) == '\0')) {
//*word = '\0';
//return TtsYes;
//}
//}
//
///*  IF HERE, NO 's FOUND, RETURN FAILURE  */
//return TtsNo;
//}
//
///******************************************************************************
// *
// *       function:       safety_check
// *
// *       purpose:        Checks to make sure that there are not too many feet
//phones per chunk.  If there are, the input is split
//into two or mor chunks.
// *
// ******************************************************************************/
//void
//safety_check(std::stringstream& stream, long* stream_length)
//{
//int number_of_feet = 0, number_of_phones = 0, state = NonPhoneme;
//long last_word_pos = UndefinedPosition, last_tg_pos = UndefinedPosition;
//char last_tg_type = '0';
//char c;
//
///*  REWIND STREAM TO BEGINNING  */
//stream.seekg(0);
//
///*  LOOP THROUGH STREAM, INSERTING NEW CHUNK MARKERS IF NECESSARY  */
//while (stream.get(c) && c != '\0') {
//switch (c) {
//case '%':
///*  IGNORE SUPER RAW MODE CONTENTS  */
//while (stream.get(c) && c != '%') {
//if (c == '\0') {
//stream.unget();
//break;
//}
//}
//state = NonPhoneme;
//break;
//case '/':
///*  SLASH CODES  */
//if (!stream.get(c)) {
//THROW_EXCEPTION(GS::EndOfBufferException, "Could not get a character from the stream.");
//}
//switch (c) {
//case 'c':
///*  CHUNK MARKER (/c)  */
//number_of_feet = number_of_phones = 0;
//break;
//case '_':
//case '*':
///*  FOOT AND TONIC FOOT MARKERS  */
//if (++number_of_feet > MaxFeetPerChunk) {
///*  SPLIT STREAM INTO TWO CHUNKS  */
//insert_chunk_marker(stream, last_word_pos, last_tg_type);
//set_tone_group(stream, last_tg_pos, ",");
//check_tonic(stream, last_tg_pos, last_word_pos);
//}
//break;
//case 't':
///*  IGNORE TAGGING MODE CONTENTS  */
///*  SKIP WHITE  */
//while (stream.get(c) && c == ' ')
//;
//stream.unget();
///*  SKIP OVER TAG NUMBER  */
//while (stream.get(c) && c != ' ') {
//if (c == '\0') {
//stream.unget();
//break;
//}
//}
//break;
//case '0':
//case '1':
//case '2':
//case '3':
//case '4':
///*  REMEMBER TONE GROUP TYPE AND POSITION  */
//last_tg_type = c;
//last_tg_pos = static_cast<long>(stream.tellg()) - 2;
//break;
//default:
///*  IGNORE ALL OTHER SLASH CODES  */
//break;
//}
//state = NonPhoneme;
//break;
//case '.':
//case '_':
//case ' ':
///*  END OF PHONE (AND WORD) DELIMITERS  */
//if (state == Phoneme) {
//if (++number_of_phones > MaxPhonesPerChunk) {
///*  SPLIT STREAM INTO TWO CHUNKS  */
//insert_chunk_marker(stream, last_word_pos, last_tg_type);
//set_tone_group(stream, last_tg_pos, ",");
//check_tonic(stream, last_tg_pos, last_word_pos);
//state = NonPhoneme;
//break;
//}
//if (c == ' ') {
//last_word_pos = static_cast<long>(stream.tellg());
//}
//}
//state = NonPhoneme;
//break;
//default:
//state = Phoneme;
//break;
//}
//}
//
///*  BE SURE TO RESET LENGTH OF STREAM  */
//*stream_length = static_cast<long>(stream.tellg());
//}
//
///******************************************************************************
// *
// *       function:       insert_chunk_marker
// *
// *       purpose:        Insert chunk markers and associated markers in the
//stream at the insert point.  Use the tone group type
//passed in as an argument.
// *
// ******************************************************************************/
//void
//insert_chunk_marker(std::stringstream& stream, long insert_point, char tg_type)
//{
//char c;
//std::stringstream temp_stream;
//
///*  COPY STREAM FROM INSERT POINT TO END TO BUFFER TO ANOTHER STREAM  */
//stream.seekg(insert_point);
//while (stream.get(c) && c != '\0') {
//temp_stream << c;
//}
//temp_stream << '\0';
//
///*  PUT IN MARKERS AT INSERT POINT  */
//stream.seekp(insert_point);
//stream << ToneGroupBoundary << ' ' << ChunkBoundary << ' '
//<< ToneGroupBoundary << " /" << tg_type << ' ';
//long new_position = static_cast<long>(stream.tellp()) - 9; //TODO: check
//
///*  APPEND CONTENTS OF TEMPORARY STREAM  */
//temp_stream.seekg(0);
//while (temp_stream.get(c) && c != '\0') {
//stream << c;
//}
//stream << '\0';
//
///*  POSITION THE STREAM AT THE NEW /c MARKER  */
//stream.seekp(new_position);
//}
//
///******************************************************************************
// *
// *       function:       check_tonic
// *
// *       purpose:        Checks to see if a tonic marker is present in the
//stream between the start and end positions.  If no
//tonic is present, then put one in at the last foot
//marker if it exists.
// *
// ******************************************************************************/
//void
//check_tonic(std::stringstream& stream, long start_pos, long end_pos)
//{
//long i, last_foot_pos = UndefinedPosition;
//
///*  REMEMBER CURRENT POSITION IN STREAM  */
//long temp_pos = static_cast<long>(stream.tellp());
//
///*  CALCULATE EXTENT OF STREAM TO LOOP THROUGH  */
//long extent = end_pos - start_pos;
//
///*  REWIND STREAM TO START POSITION  */
//stream.seekg(start_pos);
//
///*  LOOP THROUGH STREAM, DETERMINING LAST FOOT POSITION, AND PRESENCE OF TONIC  */
//char c;
//for (i = 0; i < extent; i++) {
//if (stream.get(c) && c == '/' && ++i < extent) {
//if (!stream.get(c)) {
//THROW_EXCEPTION(GS::EndOfBufferException, "Could not get a character from the stream.");
//}
//switch (c) {
//case '_':
//last_foot_pos = static_cast<long>(stream.tellg()) - 1;
//break;
//case '*':
///*  GO TO ORIGINAL POSITION ON STREAM, AND return TtsParserFailure, iMMEDIATELY  */
////NXSeek(stream, temp_pos, NX_FROMSTART);
//return;
//}
//}
//}
//
///*  IF HERE, NO TONIC, SO INSERT TONIC MARKER  */
//if (last_foot_pos != UndefinedPosition) {
//stream.seekp(last_foot_pos);
//stream << '*';
//}
//
///*  GO TO ORIGINAL POSITION ON STREAM  */
//stream.seekp(temp_pos);
//}
//
//} /* namespace */
//
////==============================================================================
//
//namespace GS {
//namespace En {
//
//TextParser::TextParser(const char* configDirPath,
//const std::string& dictionary1Path,
//const std::string& dictionary2Path,
//const std::string& dictionary3Path)
//: escape_character_(DefaultEscapeCharacter)
//{
//if (dictionary1Path != "none") {
//dict1_.reset(new DictionarySearch);
//std::ostringstream filePath;
//filePath << configDirPath << '/' << dictionary1Path;
//dict1_->load(filePath.str().c_str());
//}
//if (dictionary2Path != "none") {
//dict2_.reset(new DictionarySearch);
//std::ostringstream filePath;
//filePath << configDirPath << '/' << dictionary2Path;
//dict2_->load(filePath.str().c_str());
//}
//if (dictionary3Path != "none") {
//dict3_.reset(new DictionarySearch);
//std::ostringstream filePath;
//filePath << configDirPath << '/' << dictionary3Path;
//dict3_->load(filePath.str().c_str());
//}
//
//dictionaryOrder_[0] = TtsNumberParser;
//dictionaryOrder_[1] = TtsDictionary1;
//dictionaryOrder_[2] = TtsDictionary2;
//dictionaryOrder_[3] = TtsDictionary3;
//dictionaryOrder_[4] = TtsLetterToSound;
//dictionaryOrder_[5] = TtsEmpty;
//}
//
//TextParser::~TextParser()
//{
//}
//
///******************************************************************************
// *
// *       function:       init_parser_module
// *
// *       purpose:        Sets up parser module for subsequent use.  This must
//be called before parser() is ever used.
// *
// ******************************************************************************/
//void
//TextParser::init_parser_module()
//{
//auxStream_.str("");
//}
//
///******************************************************************************
// *
// *       function:       set_escape_code
// *
// *       purpose:        Sets escape code for parsing.  Assumes Objective C
//client library checks validity of argument.
// *
// ******************************************************************************/
//int
//TextParser::set_escape_code(char new_escape_code)
//{
///*  SET GLOBAL ESCAPE CHARACTER  */
//escape_character_ = new_escape_code;
//
///*  RETURN SUCCESS  */
//return TtsParserSuccess;
//}

// ParseText takes plain english input, and produces phonetic suitable for further processing in the TTS
// system.  If a parse error occurs, a value of 0 or above is returned.  Usually this will point to the
// position of the error in the input buffer, but in later stages of the parse only a 0 is returned since
// positional information is lost.  If no parser error, then TtsParserSuccess is returned.
func (tp *TextParser) ParseText2(rawtext string) string {
	//buf1 := make([]rune, len(rawtext)+1)
	//buf2 := make([]rune, len(rawtext)+1)

	conditioned := tp.ConditionInput(rawtext)

	e, modedText := tp.MarkModes(conditioned)
	if e != TtsParserSuccess {
		log.Fatal("Error in mark_modes();")
	}
	fmt.Println(modedText)

	//s1 := make([]rune, buf2Len)
	s2 := make([]rune, 0)
	//cleanBuf := StripPunctuation(buf2)

	//err := tp.FinalConversion(s1, &s2)
	//if err != TtsParserSuccess {
	//	panic(errors.New("Error in FinalConversion!"))
	//}

	/*  DO SAFETY CHECK;  MAKE SURE NOT TOO MANY FEET OR PHONES PER CHUNK  */
	// safety_check(auxStream_, &auxStream_length)

	phoneticString := string(s2)
	return phoneticString
}

//
///******************************************************************************
// *
// *       function:       lookup_word
// *
// *       purpose:        Returns the pronunciation of word, and sets dict to
//the dictionary in which it was found.  Relies on the
//global dictionaryOrder.
// *
// ******************************************************************************/
//const char*
//TextParser::lookup_word(const char* word, short* dict)
//{
//if (Log::debugEnabled) {
//printf("lookup_word word: %s\n", word);
//}
//
///*  SEARCH DICTIONARIES IN USER ORDER TILL PRONUNCIATION FOUND  */
//for (int i = 0; i < DICTIONARY_ORDER_SIZE; i++) {
//switch(dictionaryOrder_[i]) {
//case TtsEmpty:
//break;
//case TtsNumberParser:
//{
//const char* pron = numberParser_.parseNumber(word, NumberParser::NORMAL);
//if (pron != nullptr) {
//*dict = TtsNumberParser;
//return pron;
//}
//}
//break;
//case TtsDictionary1:
//if (dict1_) {
//const char* entry = dict1_->getEntry(word);
//if (entry != nullptr) {
//*dict = TtsDictionary1;
//return entry;
//}
//}
//break;
//case TtsDictionary2:
//if (dict2_) {
//const char* entry = dict2_->getEntry(word);
//if (entry != nullptr) {
//*dict = TtsDictionary2;
//return entry;
//}
//}
//break;
//case TtsDictionary3:
//if (dict3_) {
//const char* entry = dict3_->getEntry(word);
//if (entry != nullptr) {
//*dict = TtsDictionary3;
//return entry;
//}
//}
//break;
//default:
//break;
//}
//}
//
///*  IF HERE, THEN FIND WORD IN LETTER-TO-SOUND RULEBASE  */
///*  THIS IS GUARANTEED TO FIND A PRONUNCIATION OF SOME SORT  */
//letter_to_sound(word, pronunciation_);
//if (!pronunciation_.empty()) {
//*dict = TtsLetterToSound;
//return &pronunciation_[0];
//} else {
//*dict = TtsLetterToSound;
//return numberParser_.degenerateString(word);
//}
//}
//

// ConditionInput converts all non-printable characters (except escape)
// character to blanks.  Also connects words hyphenated over a newline.
func (tp *TextParser) ConditionInput(input string) (buf []rune) {
	j := 0
	length := len(input)
	for i := 0; i < length; i++ {
		if input[i] == '-' && (i-1) >= 0 && unicode.IsLetter(rune(input[i-1])) {
			// connect hyphenated word over newline
			ii := i
			// ignore any white space up to newline
			for {
				if ii+1 < length && input[ii+1] != '\n' && rune(input[ii+1]) != tp.Escape && unicode.IsSpace(rune(input[ii+1])) {
					ii++
				} else {
					break
				}
			}
			// if newline, then concatenate word
			if ii+1 < length && input[ii+1] == '\n' {
				i = ii
				ii++
				for { // ignore whitespace
					if i+1 < length && rune(input[i+1]) != tp.Escape && unicode.IsSpace(rune(input[i+1])) {
						i++
					} else {
						break
					}
				}
			} else { // output hyphen
				buf[j] = rune(input[i])
				j++
			}
		} else if unicode.IsLetter(rune(input[i])) && !unicode.IsPrint(rune(input[i])) && rune(input[i]) != tp.Escape {
			buf[j] = ' '
		} else {
			if j == len(buf) {
				buf = append(buf, rune(input[i]))
			} else {
				buf[j] = rune(input[i])
			}
			j++
		}
	}
	//buf = append(buf, rune('\x00'))
	return buf
}

// MarkModes parses input for modes, checking for errors, and marks output with mode start and end points.
// Tagging and silence mode arguments are checked.
func (tp *TextParser) MarkModes(input []rune) (err int, output []rune) {
	var modeStack [ModeNestMax]int
	sp := 0 // stack "pointer"
	j := 0

	modeMarker := [5][2]rune{{RawModeBegin, RawModeEnd},
		{LetterModeBegin, LetterModeEnd},
		{EmphasisModeBegin, EmphasisModeEnd},
		{TaggingModeBegin, TaggingModeEnd},
		{SilenceModeBegin, SilenceModeEnd}}

	modeStack[sp] = NormalMode

	length := len(input)
	for i := 0; i < length; i++ {
		if input[i] == tp.Escape {
			if modeStack[sp] == RawMode {
				r1 := unicode.ToLower(input[i+1])
				r2 := unicode.ToLower(input[i+2])
				if i+2 < length && (r1 == 'r' && r2 == 'e') {
					sp--
					if sp < 0 {
						return TtsParserFailure, output
					}
					output[j] = modeMarker[RawMode][End]
					j++
					i += 2
					if modeStack[sp] != NormalMode {
						output[j] = modeMarker[modeStack[sp]][Begin]
						j++
					}
				} else {
					if unicode.IsPrint(tp.Escape) {
						output[j] = tp.Escape
						j++
					}
				}
			} else { // any other mode
				if i+1 < length && input[i+1] == tp.Escape {
					if unicode.IsPrint(tp.Escape) {
						output[j] = tp.Escape
						j++
					}
					i++
				} else if (i+2 < length) && (input[i+2] == 'b' || input[i+2] == 'B') {
					var mode int
					r := unicode.ToLower(input[i+1])
					switch r {
					case 'r':
						mode = RawMode
					case 'l':
						mode = LetterMode
					case 'e':
						mode = EmphasisMode
					case 't':
						mode = TaggingMode
					case 's':
						mode = SilenceMode
					default:
						mode = UndefinedMode
					}
					if mode != UndefinedMode {
						if modeStack[sp] != NormalMode {
							output[j] = modeMarker[modeStack[sp]][End]
							j++
						}
						sp++
						if sp >= ModeNestMax {
							return TtsParserFailure, output
						}
						modeStack[sp] = mode
						output[j] = modeMarker[mode][Begin]
						i += 2
						if mode == TaggingMode {
							for i+1 < length && input[i+1] == ' ' {
								i++
							}
							minus := 0
							pos := minus
							for i+1 < length && input[i+1] != ' ' && input[i+1] != tp.Escape {
								i++
								if !(unicode.IsDigit(input[i])) && (!(input[i] == '-') || input[i] == '+') {
									return TtsParserFailure, output
								}
								if pos > 0 && (input[i] == '-' || input[i] == '+') {
									return TtsParserFailure, output
								}
								output[j] = input[i]
								j++
								if input[i] == '-' || input[i] == '+' {
									minus++
								}
								pos++
							}
							if minus >= pos {
								return TtsParserFailure, output
							}
							for i+1 < length && input[i+1] == ' ' {
								i++
							}
							t := unicode.ToLower(input[i+2])
							e := unicode.ToLower(input[i+3])
							if !(i+3 < length && input[i+1] == tp.Escape && (input[i+2] == 't') &&
								t == 't' && e == 'e') {
								output[j] = modeMarker[mode][End]
								j++
								sp--
								if sp < 0 {
									return TtsParserFailure, output
								}
								if modeStack[sp] != NormalMode {
									output[j] = modeMarker[modeStack[sp]][Begin]
								}
							}
						} else if mode == SilenceMode {
							for i+1 < length && input[i+1] == ' ' {
								i++
							}
							/*  COPY NUMBER, CHECKING VALIDITY  */
							period := 0
							for (i+1 < length && input[i+1] != ' ') && input[i+1] != tp.Escape {
								i++
								/*  ALLOW ONLY DIGITS AND PERIOD  */
								if !unicode.IsDigit(input[i]) && input[i] != '.' {
									return TtsParserFailure, output
								}
								/*  ALLOW ONLY ONE PERIOD  */
								if period > 0 && input[i] == '.' {
									return TtsParserFailure, output
								}
								/*  OUTPUT CHARACTER, KEEPING TRACK OF # OF PERIODS  */
								output[j] = input[i]
								j++
								if input[i] == '.' {
									period++
								}
							}
							for i+1 < length && input[i+1] == ' ' {
								i++
							}
							s := unicode.ToLower(input[i+2])
							e := unicode.ToLower(input[i+3])
							if !(i+3 < length && input[i+1] == tp.Escape && (input[i+2] == 't') &&
								s == 's' && e == 'e') {
								output[j] = modeMarker[mode][End]
								j++
								sp--
								if sp < 0 {
									return TtsParserFailure, output
								}
								if modeStack[sp] != NormalMode {
									output[j] = modeMarker[modeStack[sp]][Begin]
									j++
								}
							}
						}
					} else {
						if unicode.IsPrint(tp.Escape) {
							output[j] = tp.Escape
							j++
						}
					}
				} else if i+2 < length && (input[i+2] == 'e' || input[i+2] == 'E') {
					var mode int
					r := unicode.ToLower(input[i+1])
					switch r {
					case 'r':
						mode = RawMode
					case 'l':
						mode = LetterMode
					case 'e':
						mode = EmphasisMode
					case 't':
						mode = TaggingMode
					case 's':
						mode = SilenceMode
					default:
						mode = UndefinedMode
					}
					if mode != UndefinedMode {
						if modeStack[sp] != mode {
							return TtsParserFailure, output
						} else {
							sp--
							if sp < 0 {
								return TtsParserFailure, output
							}
							output[j] = modeMarker[mode][End]
							j++
							i += 2
							if modeStack[sp] != NormalMode {
								output[j] = modeMarker[modeStack[sp]][Begin]
								j++
							}
						}
					} else {
						if unicode.IsPrint(tp.Escape) {
							output[j] = tp.Escape
							j++
						}
					}
				} else {
					if unicode.IsPrint(tp.Escape) {
						output[j] = tp.Escape
						j++
					}
				}
			}
		} else {
			if j == len(output) {
				output = append(output, rune(input[i]))
			} else {
				output[j] = rune(input[i])
			}
			j++
		}
	}
	//output = append(output, rune('\x00'))
	return TtsParserSuccess, output
}

// FinalConversion converts contents of stream1 to stream2.  Adds chunk, tone group, and associated markers;
// expands words to pronunciations, and also expands other modes.
func (tp *TextParser) FinalConversion(s1 []rune, s2 *[]rune) int {
	//
	//lastWordEnd := UndefinedPosition
	//tgMarkerPos := UndefinedPosition
	//mode := NormalMode
	//nextMode := 0
	//priorTonic := TtsFalse
	//rawModeFag := TtsFalse
	//
	//lastWrittenState := StateBegin
	//var curState int
	//var nextState int
	//
	//word := make([]rune, WordLengthMax+1)
	//
	////std::string stream1String = stream1.str();
	////const char* input = stream1String.data();
	//
	//for i := 0; i < len(s1); i++ {
	//	switch s1[i] {
	//	case RawModeBegin:
	//		mode = RawMode
	//	case LetterModeBegin:
	//		mode = LetterMode
	//	case EmphasisModeBegin:
	//		mode = EmphasisMode
	//	case TaggingModeBegin:
	//		mode = TaggingMode
	//	case SilenceModeBegin:
	//		mode = SilenceMode
	//	case RawModeEnd:
	//		fallthrough
	//	case LetterModeEnd:
	//		fallthrough
	//	case EmphasisModeEnd:
	//		fallthrough
	//	case TaggingModeEnd:
	//		fallthrough
	//	case SilenceModeEnd:
	//		mode = NormalMode
	//	default:
	//		r := GetState(s1, &i, len(s1), &mode, &nextMode, &curState, nextState, &rawModeFlag, word, s2)
	//		if r != TtsParserSuccess {
	//			return TtsParserFailure, output
	//		}
	//		switch curState {
	//		case StateWord:
	//			switch lastWrittenState {
	//			case StateBegin:
	//				stream2 << CHUNK_BOUNDARY << ' '
	//			case StateFinalPunc:
	//				stream2 << TONE_GROUP_BOUNDARY << ' '
	//				prior_tonic = TTS_FALSE
	//			case StateMedialPunc:
	//				stream2 << TG_UNDEFINED << ' '
	//				tg_marker_pos = static_cast < long > (stream2.tellp())-3
	//			case StateSilence:
	//				stream2 << UTTERANCE_BOUNDARY << ' '
	//			}
	//		}
	//	}
	//}

	return 0
}

//
///*  REWIND STREAM2 BACK TO BEGINNING  */
//stream2.str("");
//
///*  GET MEMORY BUFFER ASSOCIATED WITH STREAM1  */
//std::string stream1String = stream1.str();
//const char* input = stream1String.data();
//
///*  MAIN LOOP  */
//for (i = 0; i < stream1_length; i++) {
//switch (input[i]) {
//case RawModeBegin:      mode = RawMode;      break;
//case LetterModeBegin:   mode = LetterMode;   break;
//case EmphasisModeBegin: mode = EmphasisMode; break;
//case TaggingModeBegin:  mode = TaggingMode;  break;
//case SilenceModeBegin:  mode = SilenceMode;  break;
//
//case RawModeEnd:
//case LetterModeEnd:
//case EmphasisModeEnd:
//case TaggingModeEnd:
//case SilenceModeEnd:    mode = NormalMode;   break;
//
//default:
///*  GET STATE INFORMATION  */
//if (get_state(input, &i, stream1_length, &mode, &next_mode, &current_state,
//&next_state, &raw_mode_flag, word, stream2) != TtsParserSuccess) {
//return TtsParserFailure;
//}
//
//#if 0
//printf("last_written_state = %-d current_state = %-d next_state = %-d ",
//last_written_state,current_state,next_state);
//printf("mode = %-d next_mode = %-d word = %s\n",
//mode,next_mode,word);
//#endif
//
///*  ACTION ACCORDING TO CURRENT STATE  */
//switch (current_state) {
//
//case StateWORD:
///*  ADD BEGINNING MARKERS IF NECESSARY (SWITCH FALL-THRU DESIRED)  */
//switch(last_written_state) {
//case STATEBegin:
//stream2 << ChunkBoundary << ' ';
//case StateFinalPunc:
//stream2 << ToneGroupBoundary << ' ';
//prior_tonic = TtsFalse;
//case StateMedialPunc:
//stream2 << TgUndefined << ' ';
//tg_marker_pos = static_cast<long>(stream2.tellp()) - 3;
//case StateSilence:
//stream2 << UtteranceBoundary << ' ';
//}
//
//if (mode == NormalMode) {
///*  PUT IN WORD MARKER  */
//stream2 << WORDBegin << ' ';
///*  ADD LAST WORD MARKER AND TONICIZATION IF NECESSARY  */
//switch(next_state) {
//case StateMedialPunc:
//case StateFinalPunc:
//case StateEnd:
///*  PUT IN LAST WORD MARKER  */
//stream2 << LAST_WORD << ' ';
///*  WRITE WORD TO STREAM WITH TONIC IF NO PRIOR TONICIZATION  */
//expand_word(word, (!prior_tonic), stream2);
//break;
//default:
///*  WRITE WORD TO STREAM WITHOUT TONIC  */
//expand_word(word, TtsNo, stream2);
//break;
//}
//} else if (mode == EmphasisMode) {
///*  START NEW TONE GROUP IF PRIOR TONIC ALREADY SET  */
//if (prior_tonic) {
//if (set_tone_group(stream2, tg_marker_pos, ",") == TtsParserFailure) {
//return TtsParserFailure;
//}
//stream2 << ToneGroupBoundary << ' ' << TgUndefined << ' ';
//tg_marker_pos = static_cast<long>(stream2.tellp()) - 3;
//}
///*  PUT IN WORD MARKER  */
//stream2 << WORDBegin << ' ';
///*  MARK LAST WORD OF TONE GROUP, IF NECESSARY  */
//if ((next_state == StateMedialPunc) ||
//(next_state == StateFinalPunc) ||
//(next_state == StateEnd) ||
//((next_state == StateWORD) && (next_mode == EmphasisMode)) ) {
//stream2 << LAST_WORD << ' ';
//}
///*  TONICIZE WORD  */
//expand_word(word, TtsYes, stream2);
//prior_tonic = TtsTrue;
//}
//
///*  SET LAST WRITTEN STATE, AND END POSITION AFTER THE WORD  */
//last_written_state = StateWORD;
//last_word_end = static_cast<long>(stream2.tellp());
//break;
//
//case StateMedialPunc:
///*  APPEND LAST WORD MARK, PAUSE, TONE GROUP MARK (FALL-THRU DESIRED)  */
//switch(last_written_state) {
//case StateWORD:
//if (shift_silence(input, i, stream1_length, mode, stream2)) {
//last_word_end = static_cast<long>(stream2.tellp());
//} else if ((next_state != StateEnd) &&
//another_word_follows(input, i, stream1_length, mode)) {
//if (!strcmp(word,",")) {
//stream2 << UtteranceBoundary << ' ' << MedialPause << ' ';
//} else {
//stream2 << UtteranceBoundary << ' ' << LongMedialPause << ' ';
//}
//} else if (next_state == StateEnd) {
//stream2 << UtteranceBoundary << ' ';
//}
//case StateSilence:
//stream2 << ToneGroupBoundary << ' ';
//prior_tonic = TtsFalse;
//if (set_tone_group(stream2, tg_marker_pos, word) == TtsParserFailure) {
//return TtsParserFailure;
//}
//tg_marker_pos = UndefinedPosition;
//last_written_state = StateMedialPunc;
//}
//break;
//
//case StateFinalPunc:
//if (last_written_state == StateWORD) {
//if (shift_silence(input, i, stream1_length, mode, stream2)) {
//last_word_end = static_cast<long>(stream2.tellp());
//stream2 << ToneGroupBoundary << ' ';
//prior_tonic = TtsFalse;
//if (set_tone_group(stream2, tg_marker_pos, word) == TtsParserFailure) {
//return TtsParserFailure;
//}
//tg_marker_pos = UndefinedPosition;
///*  IF SILENCE INSERTED, THEN CONVERT FINAL PUNCTUATION TO MEDIAL  */
//last_written_state = StateMedialPunc;
//} else {
//stream2 << UtteranceBoundary << ' '
//<< ToneGroupBoundary << ' ' << ChunkBoundary << ' ';
//prior_tonic = TtsFalse;
//if (set_tone_group(stream2, tg_marker_pos, word) == TtsParserFailure) {
//return TtsParserFailure;
//}
//tg_marker_pos = UndefinedPosition;
//last_written_state = StateFinalPunc;
//}
//} else if (last_written_state == StateSilence) {
//stream2 << ToneGroupBoundary << ' ';
//prior_tonic = TtsFalse;
//if (set_tone_group(stream2, tg_marker_pos, word) == TtsParserFailure) {
//return TtsParserFailure;
//}
//tg_marker_pos = UndefinedPosition;
///*  IF SILENCE INSERTED, THEN CONVERT FINAL PUNCTUATION TO MEDIAL  */
//last_written_state = StateMedialPunc;
//}
//break;
//
//case StateSilence:
//if (last_written_state == STATEBegin) {
//stream2 << ChunkBoundary << ' ' << ToneGroupBoundary << ' ' << TgUndefined << ' ';
//prior_tonic = TtsFalse;
//tg_marker_pos = static_cast<long>(stream2.tellp()) - 3;
//if ((convert_silence(word, stream2) <= 0.0) && (next_state == StateEnd)) {
//return TtsParserFailure;
//}
//last_written_state = StateSilence;
//last_word_end = static_cast<long>(stream2.tellp());
//} else if (last_written_state == StateWORD) {
//convert_silence(word, stream2);
//last_written_state = StateSilence;
//last_word_end = static_cast<long>(stream2.tellp());
//}
//break;
//
//case StateTagging:
//insert_tag(stream2, last_word_end, word);
//last_word_end = UndefinedPosition;
//break;
//
//case StateEnd:
//break;
//}
//break;
//}
//}
//
///*  FINAL STATE  */
//switch (last_written_state) {
//
//case StateMedialPunc:
//stream2 << ChunkBoundary;
//break;
//
//case StateWord:  /*  FALL THROUGH DESIRED  */
//stream2 << UtteranceBoundary << ' ';
//case StateSilence:
//stream2 << ToneGroupBoundary << ' ' << ChunkBoundary;
//prior_tonic = TtsFalse;
//if (set_tone_group(stream2, tg_marker_pos, DefaultEndPunc) == TtsParserFailure) {
//return TtsParserFailure;
//}
//tg_marker_pos = UndefinedPosition;
//break;
//
//case STATEBegin:
//if (!raw_mode_flag)
//return(TtsParserFailure);
//break;
//}
//
///*  BE SURE TO ADD NULL TO END OF STREAM  */
//stream2 << '\0';
//
///*  SET STREAM2 LENGTH  */
//*stream2_length = static_cast<long>(stream2.tellp());
//
///*  RETURN SUCCESS  */
//return TtsParserSuccess;
//}
//
///******************************************************************************
// *
// *       function:       expand_word
// *
// *       purpose:        Write pronunciation of word to stream.  Deal with
//possessives if necessary.  Also, deal with single
//characters, and upper case words (including special
//acronyms) if necessary.  Add special marks if word
//is tonic.
// *
// ******************************************************************************/
//void
//TextParser::expand_word(char* word, int is_tonic, std::stringstream& stream)
//{
//short dictionary;
//const char *pronunciation, *ptr;
//long last_foot_begin;
//int possessive = TtsNo;
//char last_phoneme[SymbolLengthMax+1], *last_phoneme_ptr;
//
///*  STRIP OF POSSESSIVE ENDING IF WORD ENDS WITH 's, SET FLAG  */
//possessive = is_possessive(word);
//
///*  USE degenerate_string IF WORD IS A SINGLE CHARACTER
//    (EXCEPT SMALL, NON-POSSESSIVE A)  */
//if ((strlen(word) == 1) && isalpha(word[0])) {
//if (!strcmp(word,"a") && !possessive) {
//pronunciation = "uh";
//} else {
//pronunciation = numberParser_.degenerateString(word);
//}
//dictionary = TtsLetterToSound;
//} else if (is_all_upper_case(word)) {
///*  ALL UPPER CASE WORDS PRONOUNCED ONE LETTER AT A TIME,
//    EXCEPT SPECIAL ACRONYMS  */
//
//if (!(pronunciation = is_special_acronym(word))) {
//pronunciation = numberParser_.degenerateString(word);
//}
//
//dictionary = TtsLetterToSound;
//} else { /*  ALL OTHER WORDS ARE LOOKED UP IN DICTIONARIES, AFTER CONVERTING TO LOWER CASE  */
//pronunciation = lookup_word((const char *)to_lower_case(word), &dictionary);
//}
//
///*  ADD FOOT BEGIN MARKER TO FRONT OF WORD IF IT HAS NO PRIMARY STRESS AND IT IS
//    TO RECEIVE A TONIC;  IF ONLY A SECONDARY STRESS MARKER, CONVERT TO PRIMARY  */
//last_foot_begin = UndefinedPosition;
//if (is_tonic && !contains_primary_stress(pronunciation)) {
//if (!converted_stress((char *)pronunciation)) {
//stream << FootBegin;
//last_foot_begin = static_cast<long>(stream.tellp()) - 2;
//}
//}
//
///*  PRINT PRONUNCIATION TO STREAM, UP TO WORD TYPE MARKER (%)  */
///*  KEEP TRACK OF LAST PHONEME  */
//ptr = pronunciation;
//last_phoneme[0] = '\0';
//last_phoneme_ptr = last_phoneme;
//while (*ptr && (*ptr != '%')) {
//switch(*ptr) {
//case '\'':
//case '`':
//stream << FootBegin;
//last_foot_begin = static_cast<long>(stream.tellp()) - 2;
//last_phoneme[0] = '\0';
//last_phoneme_ptr = last_phoneme;
//break;
//case '"':
//stream << SecondaryStress;
//last_phoneme[0] = '\0';
//last_phoneme_ptr = last_phoneme;
//break;
//case '_':
//case '.':
//stream << *ptr;
//last_phoneme[0] = '\0';
//last_phoneme_ptr = last_phoneme;
//break;
//case ' ':
///*  SUPPRESS UNNECESSARY BLANKS  */
//if (*(ptr+1) && (*(ptr+1) != ' ')) {
//stream << *ptr;
//last_phoneme[0] = '\0';
//last_phoneme_ptr = last_phoneme;
//}
//break;
//default:
//stream << *ptr;
//*last_phoneme_ptr++ = *ptr;
//*last_phoneme_ptr = '\0';
//break;
//}
//ptr++;
//}
//
///*  ADD APPROPRIATE ENDING TO PRONUNCIATION IF POSSESSIVE  */
//if (possessive) {
//if (!strcmp(last_phoneme,"p") || !strcmp(last_phoneme,"t") ||
//!strcmp(last_phoneme,"k") || !strcmp(last_phoneme,"f") ||
//!strcmp(last_phoneme,"th")) {
//stream << "_s";
//} else if (!strcmp(last_phoneme,"s") || !strcmp(last_phoneme,"sh") ||
//!strcmp(last_phoneme,"z") || !strcmp(last_phoneme,"zh") ||
//!strcmp(last_phoneme,"j") || !strcmp(last_phoneme,"ch")) {
//stream << ".uh_z";
//} else {
//stream << "_z";
//}
//}
//
///*  ADD SPACE AFTER WORD  */
//stream << ' ';
//
///*  IF TONIC, CONVERT LAST FOOT MARKER TO TONIC MARKER  */
//if (is_tonic && (last_foot_begin != UndefinedPosition)) {
//long temporaryPosition = static_cast<long>(stream.tellp());
//stream.seekp(last_foot_begin);
//stream << TonicBegin;
//stream.seekp(temporaryPosition);
//}
