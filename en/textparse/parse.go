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
*  but WITHOUT ANY WARRANTY without even the implied warranty of         *
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
*     March 7th, 1995         Fixed bug when using medial punctuation (,:)
*                             at the end of an utterance.
*
******************************************************************************/

package textparse

import (
	"bufio"
	"log"
	"math"
	"strconv"
	"unicode"
)

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
const SilencePhoneLength = 0.1 // silence phone is 100ms

const DefaultEndPunc = "."
const ModeNestMax = 100

const NonPhoneme = 0
const Phoneme = 1
const MaxPhonesPerChunk = 1500
const MaxFeetPerChunk = 100

const DefaultEscapeCharacter = 27

// Dictionary Ordering Definitions
const TtsEmpty = 0
const TtsNumberParser = 1
const TtsDictionary1 = 2
const TtsDictionary2 = 3
const TtsDictionary3 = 4
const TtsLetterToSound = 5

const TtsParserSuccess = true
const TtsParserFailure = false

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

// ParseText takes plain english input, and produces phonetic suitable for further processing in the TTS
// system.  If a parse error occurs, a value of 0 or above is returned.  Usually this will point to the
// position of the error in the input buffer, but in later stages of the parse only a 0 is returned since
// positional information is lost.  If no parser error, then TtsParserSuccess is returned.
func (tp *TextParser) ParseText2(rawtext string) string {
	//conditioned := tp.ConditionInput(rawtext)
	//
	//e, modedText := tp.MarkModes(conditioned)
	//if e != TtsParserSuccess {
	//	log.Fatal("Error in mark_modes()")
	//}
	//fmt.Println(string(modedText))
	//
	//cleanText := StripPunctuation(modedText)
	//fmt.Println(string(cleanText))
	//
	//success, converted := tp.FinalConversion(modedText)
	//if success != TtsParserSuccess {
	//	panic(errors.New("Error in FinalConversion!"))
	//}
	//fmt.Println(converted)

	/*  DO SAFETY CHECK  MAKE SURE NOT TOO MANY FEET OR PHONES PER CHUNK  */
	// safety_check(auxStream_, &auxStream_length)

	//phoneticString := string(converted)
	//return phoneticString
	return ""
}

// StripPunctuation deletes unnecessary punctuation, and converts some punctuation to another form.
func StripPunctuation(buf []rune) (output []rune) {
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
				case rune('-'):
					if !ConvertDash(buf, &i, buflen) &&
						!NumberFollows(buf, i, buflen) &&
						!IsIsolated(buf, i, buflen) {
						buf[i] = Deleted
					}
				case '+':
					if !PartOfNumber(buf, i, buflen) && !IsIsolated(buf, i, buflen) {
						buf[i] = Deleted
					}
				case '\'':
					if !(((i - 1) >= 0) && unicode.IsLetter(buf[i-1]) && ((i + 1) < buflen) && unicode.IsLetter(buf[i+1])) {
						buf[i] = Deleted
					}
				case '.':
					DeleteEllipsis(buf, &i, buflen)
				case '/', '$', '%':
					if !PartOfNumber(buf, i, buflen) {
						buf[i] = Deleted
					}
				case '<', '>', '&', '=', '@':
					if !IsIsolated(buf, i, buflen) {
						buf[i] = Deleted
					}
				case '"', '`', '#', '*', '\\', '^', '_', '|', '~', '{', '}':
					buf[i] = Deleted
				default:
				}
			}
		}
	}

	// Second pass
	mode = NormalMode
	status := Punctuation
	for i := 0; i < buflen; i++ {
		switch buf[i] {
		case RawModeBegin:
			mode = RawMode
			output = append(output, buf[i])
		case EmphasisModeBegin:
			mode = EmphasisMode
			output = append(output, buf[i])
		case TaggingModeBegin:
			mode = TaggingMode
			output = append(output, buf[i])
		case SilenceModeBegin:
			mode = SilenceMode
			output = append(output, buf[i])
		case LetterModeBegin:
			mode = LetterMode // expand below

		case RawModeEnd:
			fallthrough
		case EmphasisModeEnd:
			fallthrough
		case TaggingModeEnd:
			fallthrough
		case SilenceModeEnd:
			mode = NormalMode
			output = append(output, buf[i])
		case LetterModeEnd:
			mode = NormalMode // expand below     break
		case Deleted:
			// convert all deleted characters to blanks
			buf[i] = ' '
			output = append(output, rune(' '))
		default:
			if (mode == NormalMode) || (mode == EmphasisMode) {
				switch buf[i] {
				case '(':
					// convert (?) and (!) to blanks
					if (i+2) < buflen && buf[i+2] == ')' &&
						((buf[i+1] == '!') || (buf[i+1] == '?')) {
						buf[i+2] = ' '
						buf[i+1] = ' '
						buf[i] = ' '
						output = append(output, []rune("   ")...)
						i += 2
						continue
					}
					// allow telephone number with area code:  (403)274-3877
					if IsTelephoneNumber(buf, i, buflen) {
						for j := 0; j < 12; j++ {
							output = append(output, buf[i])
							i++
						}
						status = Word
						continue
					}
					// convert to comma if preceded by word, followed by word
					if (status == Word) && WordFollows(buf, i, buflen) {
						buf[i] = ' '
						output = append(output, []rune(", ")...)
						status = Punctuation
					} else {
						buf[i] = ' '
						output = append(output, ' ')
					}
				case ')':
					// convert to comma if preceded by word, followed by word
					if (status == Word) && WordFollows(buf, i, buflen) {
						buf[i] = ','
						output = append(output, []rune(", ")...)
						status = Punctuation
					} else {
						buf[i] = ' '
						output = append(output, ' ')
					}
					break
				case '&':
					output = append(output, []rune(And)...)
					status = Word
					break
				case '+':
					if IsIsolated(buf, i, buflen) {
						output = append(output, []rune(Plus)...)
					} else {
						output = append(output, '+')
					}
					status = Word
					break
				case '<':
					output = append(output, []rune(IsLessThan)...)
					status = Word
					break
				case '>':
					output = append(output, []rune(IsGreaterThan)...)
					status = Word
					break
				case '=':
					output = append(output, []rune(Equals)...)
					status = Word
					break
				case '-':
					if IsIsolated(buf, i, buflen) {
						output = append(output, []rune(Minus)...)
					} else {
						output = append(output, '-')
					}
					status = Word
					break
				case '@':
					output = append(output, []rune(At)...)
					status = Word
					break
				case '.':
					if !ExpandAbbreviation(buf, i, buflen, &output) {
						output = append(output, buf[i])
						status = Punctuation
					}
					break
				default:
					output = append(output, buf[i])
					if IsPunctuation(buf[i]) {
						status = Punctuation
					} else if unicode.IsDigit(buf[i]) || unicode.IsLetter(buf[i]) {
						status = Word
					}
				}
			} else if mode == LetterMode {
				// expand letter mode contents to plain words or single letters
				output = ExpandLetterMode(buf, &i, buflen, &status)
				continue
			} else { // else pass characters straight through
				output = append(output, buf[i])
			}
			break
		}
	}
	return output
}

// GetState determines the current state and next state in buffer. A word or punctuation is put into word.
// Raw mode contents are expanded and written to stream.
func (tp *TextParser) GetState(buf []rune, i *int, len int, mode, nextMode, curState, nextState,
	rawModeFlag *int, word []rune, stream []rune) bool {
	state := 0
	var curMode int
	stateBuf := []*int{curState, nextState}

	// get 2 states
	var j int
	for j = *i; j < len; j++ {
		switch buf[j] {
		case RawModeBegin:
			curMode = RawMode
			break
		case LetterModeBegin:
			curMode = LetterMode
			break
		case EmphasisModeBegin:
			curMode = EmphasisMode
			break
		case TaggingModeBegin:
			curMode = TaggingMode
			break
		case SilenceModeBegin:
			curMode = SilenceMode
			break

		case RawModeEnd:
		case LetterModeEnd:
		case EmphasisModeEnd:
		case TaggingModeEnd:
		case SilenceModeEnd:
			curMode = NormalMode
			break

		default:
			if (curMode == NormalMode) || (curMode == EmphasisMode) {
				// SKIP WHITE
				if buf[j] == ' ' {
					break
				}

				// punctuation
				if IsPunctuation(buf[j]) {
					if buf[j] == '.' && (j+1) < len && unicode.IsDigit(buf[j+1]) {
						// do nothing, handle as word later
					} else {
						// set state based on punctuation
						switch buf[j] {
						case '.', '!', '?':
							*(stateBuf[state]) = StateFinalPunc
							break
						case ';':
						case ':':
						case ',':
							*(stateBuf[state]) = StateMedialPunc
							break
						}

						// put punctuation into word buffer, set outside counter, in current state
						if state == 0 {
							word[0] = buf[j]
							word[1] = rune('\x00')
							*i = j
							// set outside mode
							*mode = curMode
						} else { // set next mode if second state
							*nextMode = curMode
						}
						// increment state
						state++
						break
					}
				}

				// word
				if state == 0 {
					k := 0
					for {
						word[k] = buf[j]
						j++
						k++
						if !(j < len && buf[j] != ' ' && !IsMode(byte(buf[j])) && k < WordLengthMax) {
							break
						}
					}
					word[k] = '\x00' // Todo: keep?
					j--

					// back up if word ends with punctuation
					for k >= 1 {
						if IsPunctuation(word[k-1]) {
							k--
							word[k] = '\x00'
							j--
						} else {
							break
						}
					}

					// e
					*i = j

					// set outside mode
					*mode = curMode
				} else {
					// SET NEXT MODE IF SECOND STATE
					*nextMode = curMode
				}

				// Set state to word, increment state
				*(stateBuf[state]) = StateWord
				state++
				break
			} else if curMode == SilenceMode && state == 0 {
				// put silence length into word buffer in current state only
				k := 0

				for {
					word[k] = buf[j]
					j++
					k++
					if !(j < len && !IsMode(byte(buf[j])) && k < WordLengthMax) {
						break
					}
				}
				word[k] = '\x00' // Todo: keep?
				j--

				*i = j
				*mode = curMode

				*(stateBuf[state]) = StateSilence
				state++
			} else if curMode == TaggingMode && state == 0 {
				// put tag into word buffer in current state only
				k := 0
				for {
					word[k] = buf[j]
					j++
					k++
					if !(j < len && !IsMode(byte(buf[j])) && k < WordLengthMax) {
						break
					}
				}
				word[k] = '\x00' // Todo: keep?
				j--
				*i = j
				*mode = curMode
				*(stateBuf[state]) = StateTagging
			} else if curMode == RawMode && state == 0 {
				// expand raw mode in current state only
				if ExpandRawMode(buf, &j, len, stream) != TtsParserSuccess {
					return (TtsParserFailure)
				}
				*rawModeFlag = TtsTrue
				*i = j
			}
			break
		}
		if state >= 2 {
			return TtsParserSuccess
		}
	}

	// IF HERE, THEN END OF INPUT BUFFER, INDICATE END STATE
	if state == 0 {
		*curState = StateEnd
		*nextState = StateUndefined
		word = word[:0]
		*i = j
		*mode = curMode
	} else {
		*nextState = StateEnd
	}
	// return success
	return TtsParserSuccess
}

// SetToneGroup sets the tone group marker according to the punctuation passed in as "word".
// The marker is inserted in the
func (tp *TextParser) SetToneGroup(stream []rune, tgPos int, word string) bool {
	if tgPos == UndefinedPosition {
		return TtsParserFailure
	}

	//long current_pos = static_cast<long>(stream.tellp())

	// seek to tone group marker position
	//stream.seekp(tgPos)

	var tg string // tonegroup
	switch word[0] {
	case '.':
		tg = TgStatement
	case '!':
		tg = TgExclamation
	case '?':
		tg = TgQuestion
	case ',':
		tg = TgContinuation
	case ';':
		tg = TgHalfPeriod
	case ':':
		tg = TgContinuation
	default:
		return TtsParserFailure
	}
	// Todo: This is sort of the idea but isn't correct I am sure!
	stream = append(stream, []rune(tg)...)
	copy(stream[tgPos+len(tg):], stream[tgPos:])
	for i := 0; i < len(tg); i++ {
		stream[tgPos+1+i] = rune(tg[i])
	}

	//SEEK TO ORIGINAL POSITION ON STREAM
	//stream.seekp(current_pos)

	return TtsParserSuccess
}

// ConvertSilence converts numeric quantity in "buffer" to appropriate number of silence phones,
// which are written onto the end of stream.  Rounding is performed.  Returns actual length of silence.
func ConvertSilence(buf []rune, out []rune) float64 {
	// convert buffer to double
	str := string(buf)
	silence, err := strconv.ParseFloat(str, 64)
	if err != nil {
		log.Println(err)
	}
	if silence > SilenceMax {
		silence = SilenceMax
	}

	// find equivalent number of silence phones, performing rounding
	phoneCount := int(math.Round(silence / SilencePhoneLength))

	out = append(out, []rune(UtteranceBoundary)...)
	out = append(out, ' ')

	// write out silence phones to streams
	for j := 0; j < phoneCount; j++ {
		out = append(out, []rune(SilencePhone)...)
		out = append(out, ' ')
	}

	// return actual length of silence
	return float64(phoneCount) * SilencePhoneLength
}

// AnotherWordFollows returns 1 if another word follows in buffer, after position i.  Else, 0 is returned
func (tp *TextParser) AnotherWordFollows(rs []rune, i, length, mode int) bool {
	for j := i + 1; j < length; j++ {
		// filter through each character
		switch rs[j] {
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
			if (mode == NormalMode) || (mode == EmphasisMode) {
				// word has been found
				if !IsPunctuation(rs[j]) {
					return true
				}
			}
		}
	}

	// IF HERE, THEN NO WORD FOLLOWS
	return false
}

// ShiftSilence  looks past punctuation to see if some silence occurs before the next word
// (or raw mode contents), and shifts the silence to the current point on the stream.  The
// the numeric quantity is converted to equivalent silence phones, and true is returned.
func (tp *TextParser) ShiftSilence(buf []rune, i, length, mode int, out []rune) bool {
	word := make([]rune, WordLengthMax+1)

	for j := i + 1; j < length; j++ {
		// filter through each character
		switch buf[j] {
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
				// skip white space
				if buf[j] == ' ' {
					continue
				}
				// word here, so return without shifting
				if !IsPunctuation(buf[j]) {
					return false
				}
			} else if mode == RawMode {
				// assume raw mode contains word of some sort
				return false
			} else if mode == SilenceMode {
				// collect silence digits into word buffer
				k := 0
				word[k] = buf[j]
				k++
				j++
				for j < length && !IsMode(byte(buf[j])) && k < WordLengthMax {
					word[k] = buf[j]
					k++
					j++
				}
				word[k] = rune('\x00')
				// convert word to silence phones, appending to stream
				ConvertSilence(word, out)
				// return, indicating silence shifted backwards
				return true
			}
			break
		}
	}

	// silence not shifted
	return false
}

// InsertTag inserts the tag contained in word onto the stream at the insert point.
func (tp *TextParser) InsertTag(rs []rune, insertPt int, word []rune) {
	if insertPt == UndefinedPosition {
		return
	}

	// find position of end of stream
	endPt := len(rs)

	// calculate how many characters to shift
	length := endPt - insertPt

	// if length is 0, th
	//en simply append tag to stream
	if length == 0 {
		s := TagBegin + " " + string(word)
		rs = append(rs, []rune(s)...)
	} else {
		// else, save stream after insert

		temp0 := rs[:insertPt]
		temp1 := rs[insertPt:]
		s := TagBegin + " " + string(word) + " "
		temp0 = append(temp0, []rune(s)...)
		temp0 = append(temp0, temp1...)

	}
}

// Todo: What's up with passing token without
// ExpandRawMode writes raw mode contents to stream, checking phones and marker
func ExpandRawMode(rs []rune, j *int, length int, stream []rune) bool {
	superRawMode := false
	delimiter := TtsFalse
	blank := TtsTrue
	var tokens []rune

	// expand and check raw mode contents till end of raw mode
	k := 0
	tokens[k] = rune('\x00')

	for ; *j < length && rs[*j] != RawModeEnd; (*j)++ {
		stream = append(stream, rs[*j])

		// check if entering or exiting super raw mode
		if rs[*j] == '%' {
			if !superRawMode {
				if IllegalToken(tokens) {
					return TtsParserFailure
				}
				superRawMode = true
				k = 0
				tokens[k] = rune('\x00')
				continue
			} else {
				superRawMode = false
				k = 0
				tokens[k] = rune('\x00')
				blank = TtsFalse
				delimiter = TtsFalse
				continue
			}
		}
		// examine slash codes, delimiters, and phones in regular raw mode
		if !superRawMode {
			switch rs[*j] {
			case '/':
				// slash code
				// evaluate pending token
				if IllegalToken(tokens) {
					return TtsParserFailure
				}
				// put slash code into token rs
				tokens[0] = '/'
				*j--
				if *j < length && rs[*j] != RawModeEnd {
					stream = append(stream, rs[*j])
					tokens[1] = rs[*j]
					tokens[2] = rune('\x00')
					// check legality of slash code
					if IllegalSlashCode(string(tokens)) {
						return TtsParserFailure
					}
					// check any tag and tag number
					if string(tokens) != TagBegin {
						if ExpandTagNumber(rs, j, length, stream) == TtsParserFailure {
							return TtsParserFailure
						}
					}
					// reset flags
					k = 0
					tokens[k] = rune('\x00')
					blank = TtsFalse
					delimiter = TtsFalse
				} else {
					return TtsParserFailure
				}
			case '_':
				fallthrough
			case '.':
				// syllable delimiters
				// don't allow repeated delimiters, or delimiters after blank
				if delimiter > 0 || blank > 0 {
					return TtsParserFailure
				}
				delimiter++
				blank = TtsFalse
				// evaluate pending token
				if IllegalToken(tokens) {
					return TtsParserFailure
				}
				// reset flags
				k = 0
				tokens[k] = rune('\x00')
			case ' ':
				// word delimiter
				// don't allow syllable delimiter before blank
				if delimiter > 0 {
					return TtsParserFailure
				}
				// set flags
				blank++
				delimiter = TtsFalse
				// evaluate pending token
				if IllegalToken(tokens) {
					return TtsParserFailure
				}
				// reset flags
				k = 0
				tokens[k] = rune('\x00')
			default:
				// phone symbol
				// reset flags
				blank = TtsFalse
				delimiter = TtsFalse
				// accumulate phone symbol in token rs
				k++
				tokens[k] = rs[*j]
				if k <= SymbolLengthMax {
					tokens[k] = rune('\x00')
				} else {
					return TtsParserFailure
				}
				break
			}
		}
		return true
	}

	// CHECK ANY REMAINING TOKENS
	if IllegalToken(tokens) {
		return TtsParserFailure
	}
	// cannot end with a delimiter
	if delimiter > 0 {
		return TtsParserFailure
	}

	// pad with space, reset external counter
	stream = append(stream, ' ')
	(*j)--

	// RETURN SUCCESS
	return TtsParserSuccess
}

// IllegalToken returns 1 if token is not a valid DEGAS phone, otherwise 0.
func IllegalToken(token []rune) bool {
	if len(token) == 0 {
		return false
	}
	// if phone a valid degas phone, return 0  1 otherwise
	//if 1 { //TODO: implement (comment from c++ version)
	//	return false
	//} /*else {
	//	return 1
	//}*/
	return false
}

// IllegalSlashCode returns true if code is illegal
func IllegalSlashCode(code string) bool {
	legalCodes := []string{ChunkBoundary, ToneGroupBoundary, FootBegin, TonicBegin, SecondaryStress, LastWord, TagBegin,
		WordBegin, TgStatement, TgExclamation, TgQuestion, TgContinuation, TgHalfPeriod}

	for _, legal := range legalCodes {
		if code == legal {
			return false
		}
	}
	return true
}

// ExpandTagNumber expand tag number in buffer at position j and write to stream.
// Perform error checking, returning error code if format of tag number is illegal.
func ExpandTagNumber(rs []rune, j *int, length int, stream []rune) bool {
	return TtsParserSuccess
}

//// SKIP WHITE
//while ((((*j)+1) < length) && (buffer[(*j)+1] == ' ')) {
//(*j)++
//stream << buffer[*j]
//}
//
//// CHECK FORMAT OF TAG NUMBER
//int sign = 0
//while ((((*j)+1) < length) && (buffer[(*j)+1] != ' ') &&
//(buffer[(*j)+1] != RawModeEnd) && (buffer[(*j)+1] != '%')) {
//stream << buffer[++(*j)]
//if ((buffer[*j] == '-') || (buffer[*j] == '+')) {
//if (sign) {
//return TtsParserFailure
//}
//sign++
//} else if (!isdigit(buffer[*j])) {
//return TtsParserFailure
//}
//}
//
//// RETURN SUCCESS
//return TtsParserSuccess
//}

// IsMode Returns 1 if character is a mode marker, otherwise 0.
func IsMode(b byte) bool {
	if int(b) >= SilenceModeEnd && int(b) <= RawModeBegin {
		return true
	}
	return false
}

// IsIsolated returns true if character at position i is isolated, i.e. is surrounded by space or mode marker.
func IsIsolated(rs []rune, i, len int) bool {
	if ((i == 0) || (((i - 1) >= 0) && (IsMode(byte(rs[i-1])) || (rs[i-1] == ' ')))) &&
		((i == (len - 1)) || (((i + 1) < len) && (IsMode(byte(rs[i+1])) || (rs[i+1] == ' ')))) {
		return true
	}
	return false
}

// PartOfNumber returns true if character at position i is part of a number
// (including mixtures with non-numeric characters)
func PartOfNumber(rs []rune, idx int, len int) bool {
	idx--
	for idx >= 0 && rs[idx] != ' ' && rs[idx] != Deleted && !IsMode(byte(rs[idx])) {
		if unicode.IsDigit(rs[idx]) {
			return true
		}
		idx--
	}
	idx++
	for idx < len && rs[idx] != ' ' && rs[idx] != Deleted && !IsMode(byte(rs[idx])) {
		if unicode.IsDigit(rs[idx]) {
			return true
		}
		idx++
	}
	return false
}

// NumberFollows returns true if at least one digit follows the character at position i,
// to white space or mode marker.
func NumberFollows(rs []rune, idx int, len int) bool {
	idx++
	for idx < len && rs[idx] != ' ' && rs[idx] != Deleted && !IsMode(byte(rs[idx])) {
		if unicode.IsDigit(rs[idx]) {
			return true
		}
	}
	return false
}

// DeleteEllipsis deletes three dots in a row (disregarding whitespace).  If four dots,
// then the last three are deleted.
func DeleteEllipsis(rs []rune, idx *int, len int) {
	// set position of first dot
	pos1 := *idx
	pos2 := 0
	pos3 := 0

	// ignore any white space
	for (*idx+1) < len && rs[*idx+1] == ' ' {
		(*idx)++
		// check for 2nd dot
		if (*idx+1) < len && rs[*idx+1] == '.' {
			*idx++
			pos2 = *idx
			// ignore any white space
			for (*idx+1) < len && rs[*idx+1] == ' ' {
				*idx++
				// check for 3rd dot
				if (*idx+1) < len && rs[*idx+1] == '.' {
					*idx++
					pos3 = *idx
					// ignore any white space
					for (*idx+1) < len && rs[*idx+1] == ' ' {
						*idx++
						// check for 4th dot
						if (*idx+1) < len && rs[*idx+1] == '.' {
							*idx++
							rs[*idx] = Deleted
							rs[pos3] = Deleted
							rs[pos2] = Deleted
						} else {
							rs[pos3] = Deleted
							rs[pos2] = Deleted
							rs[pos1] = Deleted
						}
					}
				}
			}
		}
	}
}

// ConvertDash converts "--" to ", ", and "---" to ",  ". Returns 1 if this is done, 0 otherwise.
func ConvertDash(rs []rune, idx *int, len int) bool {
	// set position of initial dash
	pos1 := *idx

	// check for 2nd dash
	if (*idx+1) < len && rs[*idx+1] == '-' {
		rs[pos1] = ','
		*idx++
		rs[*idx] = Deleted
		// check for 3rd dash
		if (*idx+1) < len && rs[*idx+1] == '-' {
			*idx++
			rs[*idx] = Deleted
		}
		return true
	}
	return false // not converted
}

// IsTelephoneNumber returns true if string at position i in buffer is of the
// form:  (ddd)ddd-dddd where each d is a digit.
func IsTelephoneNumber(rs []rune, i, len int) bool {
	// ToDo: copy some go code
	return false
}

// IsPunctuation
func IsPunctuation(r rune) bool {
	if r == rune('.') || r == ',' || r == ';' || r == ':' || r == '?' || r == '!' {
		return true
	}
	return false
}

// WordFollows returns a true if a word or speakable symbol (letter mode)  follows the position i in buffer.
// Raw, tagging, and silence mode contents are ignored.  Returns false if any punctuation (except . as part of number) follows.
func WordFollows(rs []rune, i, length int) bool {
	mode := NormalMode

	for j := i + 1; j < length; j++ {
		switch rs[j] {
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
			switch mode {
			case NormalMode:
			case EmphasisMode:
				if rs[j] == ' ' || rs[j] == Deleted {
					continue
				} else if IsPunctuation(rs[j]) {
					// punctuation means no word follows (unless period part of number)

					if (rs[j] == '.') && ((j + 1) < length) && unicode.IsDigit(rs[j+1]) {
						return true
					} else {
						return false
					}
				} else { // ELSE, SOME WORD FOLLOWS
					return true
				}
			case LetterMode:
				return true
			case RawMode:
			case SilenceMode:
			case TaggingMode:
				continue
			}
		}
	}
	return false
}

// ExpandAbbreviation expands listed abbreviations.  Two lists are used (see abbreviations.h):
// one list expands unconditionally, the other only if the abbreviation is followed by a  number.
// The abbreviation p. is expanded to page. Single alphabetic characters have periods deleted, but
// no expansion is made.  They are also capitalized. Returns 1 if expansion made (i.e. period is deleted),
func ExpandAbbreviation(rs []rune, i, length int, expanded *[]rune) bool {
	//	var word [5]rune
	//
	//	// delete period after single character (except p.)
	//	if ( ((i - 1) == 0) || ( ((i - 2) >= 0) &&
	//		((rs[i-2] == ' ') || (rs[i-2] == '.') || (IsMode(byte(rs[i-2])) ) ) ) {
	//		if unicode.IsLetter(rs[i-1]) {
	//			if (rs[i-1] == 'p') && (((i - 1) == 0) || (((i - 2) >= 0) && (rs[i-2] != '.'))) {
	//				// EXPAND p. TO page
	//				stream.seekp(-1, std::ios_base::cur)
	//				stream << "page "
	//			} else {
	//				// else, capitalize character if necessary, blank out period
	//				stream.seekp(-1, std::ios_base::cur)
	//				if islower(rs[i-1]) {
	//					rs[i-1] = toupper(rs[i-1])
	//				}
	//				stream << rs[i-1] << ' '
	//			}
	//			// indicate abbreviation expanded
	//			return 1
	//		}
	//	}
	//
	//	// get length of preceding isolated string, up to 4 characters
	//	for (j = 2 j <= 4 j++) {
	//		if ((i - j) == 0) ||
	//					(((i - (j + 1)) >= 0) && ((rs[i-(j+1)] == ' ') || (is_mode(rs[i-(j+1)])))) {
	//			if unicode.IsLetter(rs[i-j]) && unicode.IsLetter(rs[i-j+1]) {
	//				word_length = j
	//				break
	//			}
	//		}
	//	}
	//
	//	// is abbreviation only if word length is 2, 3, or 4 characters
	//	if (word_length >= 2) && (word_length <= 4) {
	//		// get abbreviation
	//		for (k = 0, j = i - word_length
	//		k < word_length
	//		k++) {
	//			word[k] = rs
	//			[j++]
	//}
	//word[k] = rune('\x00')
	//
	//// expand these abbreviations only if followed by number
	//for (j = 0 abbr_with_number[j][Abbreviation] != NULL j++) {
	//if (!strcmp(abbr_with_number[j][Abbreviation], word)) {
	//// ignore white space
	//while (((i+1) < length) && ((rs[i+1] == ' ') || (rs[i+1] == Deleted))) {
	//i++
	//}
	//// expand only if number follows
	//if (number_follows(rs, i, length)) {
	//stream.seekp(-word_length, std::ios_base::cur)
	//stream << abbr_with_number[j][Expansion] << ' '
	//return 1
	//}
	//}

	// expand these abbreviations unconditionally
	//for (j = 0 abbreviation[j][Abbreviation] != NULL j++) {
	//if (!strcmp(abbreviation[j][Abbreviation], word)) {
	//stream.seekp(-word_length, std::ios_base::cur)
	//stream << abbreviation[j][Expansion] << ' '
	//return 1
	//}
	//}
	//}

	// if here, then no expansion made
	return false
}

// ExpandLetterMode expands contents of letter mode string to word or words.  A comma is added after
// each expansion, except the last letter when it is followed by punctuation.
// cp is current position
func ExpandLetterMode(rs []rune, cp *int, len int, status *int) (output []rune) {
	for ; *cp < len && rs[*cp] != LetterModeEnd; *cp++ {
		switch rs[*cp] {
		case ' ':
			output = append(output, []rune("blank")...)
		case '!':
			output = append(output, []rune("exclamation point")...)
		case '"':
			output = append(output, []rune("double quote")...)
		case '#':
			output = append(output, []rune("number sign")...)
		case '$':
			output = append(output, []rune("dollar")...)
		case '%':
			output = append(output, []rune("percent")...)
		case '&':
			output = append(output, []rune("ampersand")...)
		case '\'':
			output = append(output, []rune("single quote")...)
		case '(':
			output = append(output, []rune("open parenthesis")...)
		case ')':
			output = append(output, []rune("close parenthesis")...)
		case '*':
			output = append(output, []rune("asterisk")...)
		case '+':
			output = append(output, []rune("plus sign")...)
		case ',':
			output = append(output, []rune("comma")...)
		case '-':
			output = append(output, []rune("hyphen")...)
		case '.':
			output = append(output, []rune("period")...)
		case '/':
			output = append(output, []rune("slash")...)
		case '0':
			output = append(output, []rune("zero")...)
		case '1':
			output = append(output, []rune("one")...)
		case '2':
			output = append(output, []rune("two")...)
		case '3':
			output = append(output, []rune("three")...)
		case '4':
			output = append(output, []rune("four")...)
		case '5':
			output = append(output, []rune("five")...)
		case '6':
			output = append(output, []rune("six")...)
		case '7':
			output = append(output, []rune("seven")...)
		case '8':
			output = append(output, []rune("eight")...)
		case '9':
			output = append(output, []rune("nine")...)
		case ':':
			output = append(output, []rune("colon")...)
		case ';':
			output = append(output, []rune("semicolon")...)
		case '<':
			output = append(output, []rune("open angle bracket")...)
		case '=':
			output = append(output, []rune("equal sign")...)
		case '>':
			output = append(output, []rune("close angle bracket")...)
		case '?':
			output = append(output, []rune("question mark")...)
		case '@':
			output = append(output, []rune("at sign")...)
		case 'a', 'A':
			output = append(output, 'A')
		case 'b', 'B':
			output = append(output, 'B')
		case 'c', 'C':
			output = append(output, 'C')
		case 'd', 'D':
			output = append(output, 'D')
		case 'e', 'E':
			output = append(output, 'E')
		case 'f', 'F':
			output = append(output, 'F')
		case 'g', 'G':
			output = append(output, 'G')
		case 'h', 'H':
			output = append(output, 'H')
		case 'i', 'I':
			output = append(output, 'I')
		case 'j', 'J':
			output = append(output, 'J')
		case 'k', 'K':
			output = append(output, 'K')
		case 'l', 'L':
			output = append(output, 'L')
		case 'm', 'M':
			output = append(output, 'M')
		case 'n', 'N':
			output = append(output, 'N')
		case 'o', 'O':
			output = append(output, 'O')
		case 'p', 'P':
			output = append(output, 'P')
		case 'q', 'Q':
			output = append(output, 'Q')
		case 'r', 'R':
			output = append(output, 'R')
		case 's', 'S':
			output = append(output, 'S')
		case 't', 'T':
			output = append(output, 'T')
		case 'u', 'U':
			output = append(output, 'U')
		case 'v', 'V':
			output = append(output, 'V')
		case 'w', 'W':
			output = append(output, 'W')
		case 'x', 'X':
			output = append(output, 'X')
		case 'y', 'Y':
			output = append(output, 'Y')
		case 'z', 'Z':
			output = append(output, 'Z')
		case '[':
			output = append(output, []rune("open square bracket")...)
		case '\\':
			output = append(output, []rune("back slash")...)
		case ']':
			output = append(output, []rune("close square bracket")...)
		case '^':
			output = append(output, []rune("caret")...)
		case '_':
			output = append(output, []rune("under score")...)
		case '`':
			output = append(output, []rune("grave accent")...)
		case '{':
			output = append(output, []rune("open brace")...)
		case '|':
			output = append(output, []rune("vertical bar")...)
		case '}':
			output = append(output, []rune("close brace")...)
		case '~':
			output = append(output, []rune("tilde")...)
		default:
			output = append(output, []rune("unknown")...)
		}
		// append comma, unless punctuation follows last letter
		if (((*cp) + 1) < len) &&
			(rs[*cp+1] == LetterModeEnd) &&
			!WordFollows(rs, *cp, len) {
			output = append(output, ' ')
			*status = Word
		} else {
			output = append(output, []rune(", ")...)
			*status = Punctuation
		}
	}
	// be sure to set index back one, so callcpng routine not fouled up
	*cp--
	return output
}

// AllToLower
func AllToLower(word []rune) {
	for i := 0; i < len(word); i++ {
		word[i] = unicode.ToLower(word[i])
	}
}

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
//char *ptr = word
//
//while (*ptr) {
//if (isupper(*ptr))
//*ptr = tolower(*ptr)
//ptr++
//}
//
//return(word)
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
//const char* acronym
//
//// LOOP THROUGH LIST UNTIL MATCH FOUND, RETURN PRONUNCIATION
//for (int i = 0 (acronym = special_acronym[i][WORD]) i++) {
//if (!strcmp(word, acronym)) {
//return special_acronym[i][Pronounciation]
//}
//}
//
//// IF HERE, NO SPECIAL ACRONYM FOUND, RETURN NULL
//return nullptr
//}
//

// HasPrimaryStress returns 1 if the pronunciation contains ' (and ` for backwards compatibility)
func HasPrimaryStress(rs []rune) bool {
	for i := 0; i < len(rs); i++ {
		if rs[i] == '%' {
			break
		} else if rs[i] == '\'' || rs[i] == '`' {
			return true
		}
	}
	return false
}

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
//// LOOP THRU PRONUNCIATION UNTIL " FOUND, REPLACE WITH '
//for (  *pronunciation && (*pronunciation != '%') pronunciation++)
//if (*pronunciation == '"') {
//*pronunciation = '\''
//return(TtsYes)
//}
//
//// IF HERE, NO " FOUND
//return(TtsNo)
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
//// LOOP UNTIL 's FOUND, REPLACE ' WITH NULL
//for (  *word word++) {
//if ((*word == '\'') && *(word+1) && (*(word+1) == 's') && (*(word+2) == rune('\x00'))) {
//*word = rune('\x00')
//return TtsYes
//}
//}
//
//// IF HERE, NO 's FOUND, RETURN FAILURE
//return TtsNo
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
//int number_of_feet = 0, number_of_phones = 0, state = NonPhoneme
//long LastWord_pos = UndefinedPosition, last_tg_pos = UndefinedPosition
//char last_tg_type = '0'
//char c
//
//// REWIND STREAM TO BEGINNING
//stream.seekg(0)
//
//// LOOP THROUGH STREAM, INSERTING NEW CHUNK MARKERS IF NECESSARY
//while (stream.get(c) && c != rune('\x00')) {
//switch (c) {
//case '%':
//// IGNORE SUPER RAW MODE CONTENTS
//while (stream.get(c) && c != '%') {
//if (c == rune('\x00')) {
//stream.unget()
//break
//}
//}
//state = NonPhoneme
//break
//case '/':
//// SLASH CODES
//if (!stream.get(c)) {
//THROW_EXCEPTION(GS::EndOfBufferException, "Could not get a character from the stream.")
//}
//switch (c) {
//case 'c':
//// CHUNK MARKER (/c)
//number_of_feet = number_of_phones = 0
//break
//case '_':
//case '*':
//// FOOT AND TONIC FOOT MARKERS
//if (++number_of_feet > MaxFeetPerChunk) {
//// SPLIT STREAM INTO TWO CHUNKS
//insert_chunk_marker(stream, LastWord_pos, last_tg_type)
//set_tone_group(stream, last_tg_pos, ",")
//check_tonic(stream, last_tg_pos, LastWord_pos)
//}
//break
//case 't':
//// IGNORE TAGGING MODE CONTENTS
//// SKIP WHITE
//while (stream.get(c) && c == ' ')
//
//stream.unget()
//// SKIP OVER TAG NUMBER
//while (stream.get(c) && c != ' ') {
//if (c == rune('\x00')) {
//stream.unget()
//break
//}
//}
//break
//case '0':
//case '1':
//case '2':
//case '3':
//case '4':
//// REMEMBER TONE GROUP TYPE AND POSITION
//last_tg_type = c
//last_tg_pos = static_cast<long>(stream.tellg()) - 2
//break
//default:
//// IGNORE ALL OTHER SLASH CODES
//break
//}
//state = NonPhoneme
//break
//case '.':
//case '_':
//case ' ':
//// END OF PHONE (AND WORD) DELIMITERS
//if (state == Phoneme) {
//if (++number_of_phones > MaxPhonesPerChunk) {
//// SPLIT STREAM INTO TWO CHUNKS
//insert_chunk_marker(stream, LastWord_pos, last_tg_type)
//set_tone_group(stream, last_tg_pos, ",")
//check_tonic(stream, last_tg_pos, LastWord_pos)
//state = NonPhoneme
//break
//}
//if (c == ' ') {
//LastWord_pos = static_cast<long>(stream.tellg())
//}
//}
//state = NonPhoneme
//break
//default:
//state = Phoneme
//break
//}
//}
//
//// BE SURE TO RESET LENGTH OF STREAM
//*stream_length = static_cast<long>(stream.tellg())
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
//insert_chunk_marker(std::stringstream& stream, long insertPt, char tg_type)
//{
//char c
//std::stringstream temp_stream
//
//// COPY STREAM FROM INSERT POINT TO END TO BUFFER TO ANOTHER STREAM
//stream.seekg(insertPt)
//while (stream.get(c) && c != rune('\x00')) {
//temp_stream << c
//}
//temp_stream << rune('\x00')
//
//// PUT IN MARKERS AT INSERT POINT
//stream.seekp(insertPt)
//stream << ToneGroupBoundary << ' ' << ChunkBoundary << ' '
//<< ToneGroupBoundary << " /" << tg_type << ' '
//long new_position = static_cast<long>(stream.tellp()) - 9 //TODO: check
//
//// APPEND CONTENTS OF TEMPORARY STREAM
//temp_stream.seekg(0)
//while (temp_stream.get(c) && c != rune('\x00')) {
//stream << c
//}
//stream << rune('\x00')
//
//// POSITION THE STREAM AT THE NEW /c MARKER
//stream.seekp(new_position)
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
//long i, last_foot_pos = UndefinedPosition
//
//// REMEMBER CURRENT POSITION IN STREAM
//long temp_pos = static_cast<long>(stream.tellp())
//
//// CALCULATE EXTENT OF STREAM TO LOOP THROUGH
//long extent = end_pos - start_pos
//
//// REWIND STREAM TO START POSITION
//stream.seekg(start_pos)
//
//// LOOP THROUGH STREAM, DETERMINING LAST FOOT POSITION, AND PRESENCE OF TONIC
//char c
//for (i = 0 i < extent i++) {
//if (stream.get(c) && c == '/' && ++i < extent) {
//if (!stream.get(c)) {
//THROW_EXCEPTION(GS::EndOfBufferException, "Could not get a character from the stream.")
//}
//switch (c) {
//case '_':
//last_foot_pos = static_cast<long>(stream.tellg()) - 1
//break
//case '*':
//// GO TO ORIGINAL POSITION ON STREAM, AND return TtsParserFailure, iMMEDIATELY
////NXSeek(stream, temp_pos, NX_FROMSTART)
//return
//}
//}
//}
//
//// IF HERE, NO TONIC, SO INSERT TONIC MARKER
//if (last_foot_pos != UndefinedPosition) {
//stream.seekp(last_foot_pos)
//stream << '*'
//}
//
//// GO TO ORIGINAL POSITION ON STREAM
//stream.seekp(temp_pos)
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
//dict1_.reset(new DictionarySearch)
//std::ostringstream filePath
//filePath << configDirPath << '/' << dictionary1Path
//dict1_->load(filePath.str().c_str())
//}
//if (dictionary2Path != "none") {
//dict2_.reset(new DictionarySearch)
//std::ostringstream filePath
//filePath << configDirPath << '/' << dictionary2Path
//dict2_->load(filePath.str().c_str())
//}
//if (dictionary3Path != "none") {
//dict3_.reset(new DictionarySearch)
//std::ostringstream filePath
//filePath << configDirPath << '/' << dictionary3Path
//dict3_->load(filePath.str().c_str())
//}
//
//dictionaryOrder_[0] = TtsNumberParser
//dictionaryOrder_[1] = TtsDictionary1
//dictionaryOrder_[2] = TtsDictionary2
//dictionaryOrder_[3] = TtsDictionary3
//dictionaryOrder_[4] = TtsLetterToSound
//dictionaryOrder_[5] = TtsEmpty
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
//auxStream_.str("")
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
//// SET GLOBAL ESCAPE CHARACTER
//escape_character_ = new_escape_code
//
//// RETURN SUCCESS
//return TtsParserSuccess
//}

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
//printf("lookup_word word: %s\n", word)
//}
//
//// SEARCH DICTIONARIES IN USER ORDER TILL PRONUNCIATION FOUND
//for (int i = 0 i < DICTIONARY_ORDER_SIZE i++) {
//switch(dictionaryOrder_[i]) {
//case TtsEmpty:
//break
//case TtsNumberParser:
//{
//const char* pron = numberParser_.parseNumber(word, NumberParser::NORMAL)
//if (pron != nullptr) {
//*dict = TtsNumberParser
//return pron
//}
//}
//break
//case TtsDictionary1:
//if (dict1_) {
//const char* entry = dict1_->getEntry(word)
//if (entry != nullptr) {
//*dict = TtsDictionary1
//return entry
//}
//}
//break
//case TtsDictionary2:
//if (dict2_) {
//const char* entry = dict2_->getEntry(word)
//if (entry != nullptr) {
//*dict = TtsDictionary2
//return entry
//}
//}
//break
//case TtsDictionary3:
//if (dict3_) {
//const char* entry = dict3_->getEntry(word)
//if (entry != nullptr) {
//*dict = TtsDictionary3
//return entry
//}
//}
//break
//default:
//break
//}
//}
//
//// IF HERE, THEN FIND WORD IN LETTER-TO-SOUND RULEBASE
//// THIS IS GUARANTEED TO FIND A PRONUNCIATION OF SOME SORT
//letter_to_sound(word, pronunciation_)
//if (!pronunciation_.empty()) {
//*dict = TtsLetterToSound
//return &pronunciation_[0]
//} else {
//*dict = TtsLetterToSound
//return numberParser_.degenerateString(word)
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
func (tp *TextParser) MarkModes(input []rune) (success bool, output []rune) {
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
							// COPY NUMBER, CHECKING VALIDITY
							period := 0
							for (i+1 < length && input[i+1] != ' ') && input[i+1] != tp.Escape {
								i++
								// ALLOW ONLY DIGITS AND PERIOD
								if !unicode.IsDigit(input[i]) && input[i] != '.' {
									return TtsParserFailure, output
								}
								// ALLOW ONLY ONE PERIOD
								if period > 0 && input[i] == '.' {
									return TtsParserFailure, output
								}
								// OUTPUT CHARACTER, KEEPING TRACK OF # OF PERIODS
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

// FinalConversion converts contents of stream1 to stream2.  Adds chunk, tone group, and associated markers
// expands words to pronunciations, and also expands other modes.
func (tp *TextParser) FinalConversion(s1 []rune) (success bool, s2 []rune) {
	//
	//lastWordEnd := UndefinedPosition
	//tgMarkerPos := UndefinedPosition
	//mode := NormalMode
	//nextMode := 0
	//priorTonic := TtsFalse
	//rawModeFlag := TtsFalse
	//
	//lastWrittenState := StateBegin
	//var curState int
	//var nextState int
	//
	//word := make([]rune, WordLengthMax+1)
	//
	//for i := 0; i < len(s1) i++ {
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
	//			if mode == NormalMode {
	//				// put in word marker
	//				stream2 << WORD_BEGIN << ' '
	//				// add last word marker and tonicization if necessary
	//				switch nextState {
	//				case StateMedialPunc:
	//					fallthrough
	//				case StateFinalPunc:
	//					fallthrough
	//				case StateEnd:
	//					// put in last word marker
	//					stream2 << LastWord << ' '
	//					// write word to stream with tonic if no prior tonicization
	//					tp.ExpandWord(word, (!priorTonic), stream2)
	//					break
	//				default:
	//					// write word to stream without tonic
	//					tp.ExpandWord(word, TtsNo, stream2)
	//					break
	//				}
	//			} else if mode == EmphasisMode {
	//				// start new tone group if prior tonic already set
	//				if priorTonic {
	//					if tp.SetToneGroup(stream2, tgMarkerPos, ",") == TtsParserFailure {
	//						return TtsParserFailure
	//					}
	//					stream2 << ToneGroupBoundary << ' ' << TgUndefined << ' '
	//					tgMarkerPos = static_cast < long > (stream2.tellp())-3
	//				}
	//				// put in word marker
	//				stream2 << WordBegin << ' '
	//				// mark last word of tone group, if necessary
	//				if (next_state == StateMedialPunc) ||
	//										(next_state == StateFinallPunc) ||
	//										(next_state == StateEnd) ||
	//										((next_state == StateWord) && (next_mode == EmphasisMode)) {
	//					stream2 << LastWord << ' '
	//				}
	//				// tonicize word
	//				tp.ExpandWord(word, TtsYes, stream2)
	//				priorTonic = TtsTrue
	//
	//			}
	//		}
	//	}
	//}
	return success, s2
}

//
//// REWIND STREAM2 BACK TO BEGINNING
//stream2.str("")
//
//// GET MEMORY BUFFER ASSOCIATED WITH STREAM1
//std::string stream1String = stream1.str()
//const char* input = stream1String.data()
//
//// MAIN LOOP
//for (i = 0 i < stream1_length i++) {
//switch (input[i]) {
//case RawModeBegin:      mode = RawMode      break
//case LetterModeBegin:   mode = LetterMode   break
//case EmphasisModeBegin: mode = EmphasisMode break
//case TaggingModeBegin:  mode = TaggingMode  break
//case SilenceModeBegin:  mode = SilenceMode  break
//
//case RawModeEnd:
//case LetterModeEnd:
//case EmphasisModeEnd:
//case TaggingModeEnd:
//case SilenceModeEnd:    mode = NormalMode   break
//
//default:
//// GET STATE INFORMATION
//if (get_state(input, &i, stream1_length, &mode, &next_mode, &current_state,
//&next_state, &raw_mode_flag, word, stream2) != TtsParserSuccess) {
//return TtsParserFailure
//}
//
//#if 0
//printf("last_written_state = %-d current_state = %-d next_state = %-d ",
//last_written_state,current_state,next_state)
//printf("mode = %-d next_mode = %-d word = %s\n",
//mode,next_mode,word)
//#endif
//
//// ACTION ACCORDING TO CURRENT STATE
//switch (current_state) {
//
//case StateWORD:
//// ADD BEGINNING MARKERS IF NECESSARY (SWITCH FALL-THRU DESIRED)
//switch(last_written_state) {
//case STATEBegin:
//stream2 << ChunkBoundary << ' '
//case StateFinalPunc:
//stream2 << ToneGroupBoundary << ' '
//prior_tonic = TtsFalse
//case StateMedialPunc:
//stream2 << TgUndefined << ' '
//tg_marker_pos = static_cast<long>(stream2.tellp()) - 3
//case StateSilence:
//stream2 << UtteranceBoundary << ' '
//}
//
//if (mode == NormalMode) {
//// PUT IN WORD MARKER
//stream2 << WORDBegin << ' '
//// ADD LAST WORD MARKER AND TONICIZATION IF NECESSARY
//switch(next_state) {
//case StateMedialPunc:
//case StateFinalPunc:
//case StateEnd:
//// PUT IN LAST WORD MARKER
//stream2 << LastWord << ' '
//// WRITE WORD TO STREAM WITH TONIC IF NO PRIOR TONICIZATION
//expand_word(word, (!prior_tonic), stream2)
//break
//default:
//// WRITE WORD TO STREAM WITHOUT TONIC
//expand_word(word, TtsNo, stream2)
//break
//}
//} else if (mode == EmphasisMode) {
//// START NEW TONE GROUP IF PRIOR TONIC ALREADY SET
//if (prior_tonic) {
//if (set_tone_group(stream2, tg_marker_pos, ",") == TtsParserFailure) {
//return TtsParserFailure
//}
//stream2 << ToneGroupBoundary << ' ' << TgUndefined << ' '
//tg_marker_pos = static_cast<long>(stream2.tellp()) - 3
//}
//// PUT IN WORD MARKER
//stream2 << WORDBegin << ' '
//// MARK LAST WORD OF TONE GROUP, IF NECESSARY
//if ((next_state == StateMedialPunc) ||
//(next_state == StateFinalPunc) ||
//(next_state == StateEnd) ||
//((next_state == StateWORD) && (next_mode == EmphasisMode)) ) {
//stream2 << LastWord << ' '
//}
//// TONICIZE WORD
//expand_word(word, TtsYes, stream2)
//prior_tonic = TtsTrue
//}
//
//// SET LAST WRITTEN STATE, AND END POSITION AFTER THE WORD
//last_written_state = StateWORD
//LastWord_end = static_cast<long>(stream2.tellp())
//break
//
//case StateMedialPunc:
//// APPEND LAST WORD MARK, PAUSE, TONE GROUP MARK (FALL-THRU DESIRED)
//switch(last_written_state) {
//case StateWORD:
//if (shift_silence(input, i, stream1_length, mode, stream2)) {
//LastWord_end = static_cast<long>(stream2.tellp())
//} else if ((next_state != StateEnd) &&
//another_WordFollows(input, i, stream1_length, mode)) {
//if (!strcmp(word,",")) {
//stream2 << UtteranceBoundary << ' ' << MedialPause << ' '
//} else {
//stream2 << UtteranceBoundary << ' ' << LongMedialPause << ' '
//}
//} else if (next_state == StateEnd) {
//stream2 << UtteranceBoundary << ' '
//}
//case StateSilence:
//stream2 << ToneGroupBoundary << ' '
//prior_tonic = TtsFalse
//if (set_tone_group(stream2, tg_marker_pos, word) == TtsParserFailure) {
//return TtsParserFailure
//}
//tg_marker_pos = UndefinedPosition
//last_written_state = StateMedialPunc
//}
//break
//
//case StateFinalPunc:
//if (last_written_state == StateWORD) {
//if (shift_silence(input, i, stream1_length, mode, stream2)) {
//LastWord_end = static_cast<long>(stream2.tellp())
//stream2 << ToneGroupBoundary << ' '
//prior_tonic = TtsFalse
//if (set_tone_group(stream2, tg_marker_pos, word) == TtsParserFailure) {
//return TtsParserFailure
//}
//tg_marker_pos = UndefinedPosition
//// IF SILENCE INSERTED, THEN CONVERT FINAL PUNCTUATION TO MEDIAL
//last_written_state = StateMedialPunc
//} else {
//stream2 << UtteranceBoundary << ' '
//<< ToneGroupBoundary << ' ' << ChunkBoundary << ' '
//prior_tonic = TtsFalse
//if (set_tone_group(stream2, tg_marker_pos, word) == TtsParserFailure) {
//return TtsParserFailure
//}
//tg_marker_pos = UndefinedPosition
//last_written_state = StateFinalPunc
//}
//} else if (last_written_state == StateSilence) {
//stream2 << ToneGroupBoundary << ' '
//prior_tonic = TtsFalse
//if (set_tone_group(stream2, tg_marker_pos, word) == TtsParserFailure) {
//return TtsParserFailure
//}
//tg_marker_pos = UndefinedPosition
//// IF SILENCE INSERTED, THEN CONVERT FINAL PUNCTUATION TO MEDIAL
//last_written_state = StateMedialPunc
//}
//break
//
//case StateSilence:
//if (last_written_state == STATEBegin) {
//stream2 << ChunkBoundary << ' ' << ToneGroupBoundary << ' ' << TgUndefined << ' '
//prior_tonic = TtsFalse
//tg_marker_pos = static_cast<long>(stream2.tellp()) - 3
//if ((convert_silence(word, stream2) <= 0.0) && (next_state == StateEnd)) {
//return TtsParserFailure
//}
//last_written_state = StateSilence
//LastWord_end = static_cast<long>(stream2.tellp())
//} else if (last_written_state == StateWORD) {
//convert_silence(word, stream2)
//last_written_state = StateSilence
//LastWord_end = static_cast<long>(stream2.tellp())
//}
//break
//
//case StateTagging:
//insert_tag(stream2, LastWord_end, word)
//LastWord_end = UndefinedPosition
//break
//
//case StateEnd:
//break
//}
//break
//}
//}
//
//// FINAL STATE
//switch (last_written_state) {
//
//case StateMedialPunc:
//stream2 << ChunkBoundary
//break
//
//case StateWord:  // FALL THROUGH DESIRED
//stream2 << UtteranceBoundary << ' '
//case StateSilence:
//stream2 << ToneGroupBoundary << ' ' << ChunkBoundary
//prior_tonic = TtsFalse
//if (set_tone_group(stream2, tg_marker_pos, DefaultEndPunc) == TtsParserFailure) {
//return TtsParserFailure
//}
//tg_marker_pos = UndefinedPosition
//break
//
//case STATEBegin:
//if (!raw_mode_flag)
//return(TtsParserFailure)
//break
//}
//
//// BE SURE TO ADD NULL TO END OF STREAM
//stream2 << rune('\x00')
//
//// SET STREAM2 LENGTH
//*stream2_length = static_cast<long>(stream2.tellp())
//
//// RETURN SUCCESS
//return TtsParserSuccess
//}
//

// TextParser writes pronunciation of word to stream.  Deals with possessives if necessary.
// Also, deals with single characters, and upper case words (including special acronyms) if necessary.
// Add special marks if word is tonic
func (tp *TextParser) ExpandWord(word string, isTonic int, w *bufio.Writer) {

}

//{
//short dictionary
//const char *pronunciation, *ptr
//long last_foot_begin
//int possessive = TtsNo
//char last_phoneme[SymbolLengthMax+1], *last_phoneme_ptr
//
//// STRIP OF POSSESSIVE ENDING IF WORD ENDS WITH 's, SET FLAG
//possessive = is_possessive(word)
//
//// USE degenerate_string IF WORD IS A SINGLE CHARACTER
//    (EXCEPT SMALL, NON-POSSESSIVE A)
//if ((strlen(word) == 1) && unicode.IsLetter(word[0])) {
//if (!strcmp(word,"a") && !possessive) {
//pronunciation = "uh"
//} else {
//pronunciation = numberParser_.degenerateString(word)
//}
//dictionary = TtsLetterToSound
//} else if (is_all_upper_case(word)) {
//// ALL UPPER CASE WORDS PRONOUNCED ONE LETTER AT A TIME,
//    EXCEPT SPECIAL ACRONYMS
//
//if (!(pronunciation = is_special_acronym(word))) {
//pronunciation = numberParser_.degenerateString(word)
//}
//
//dictionary = TtsLetterToSound
//} else { // ALL OTHER WORDS ARE LOOKED UP IN DICTIONARIES, AFTER CONVERTING TO LOWER CASE
//pronunciation = lookup_word((const char *)to_lower_case(word), &dictionary)
//}
//
//// ADD FOOT BEGIN MARKER TO FRONT OF WORD IF IT HAS NO PRIMARY STRESS AND IT IS
//    TO RECEIVE A TONIC  IF ONLY A SECONDARY STRESS MARKER, CONVERT TO PRIMARY
//last_foot_begin = UndefinedPosition
//if (is_tonic && !HasPrimaryStress(pronunciation)) {
//if (!converted_stress((char *)pronunciation)) {
//stream << FootBegin
//last_foot_begin = static_cast<long>(stream.tellp()) - 2
//}
//}
//
//// PRINT PRONUNCIATION TO STREAM, UP TO WORD TYPE MARKER (%)
//// KEEP TRACK OF LAST PHONEME
//ptr = pronunciation
//last_phoneme[0] = rune('\x00')
//last_phoneme_ptr = last_phoneme
//while (*ptr && (*ptr != '%')) {
//switch(*ptr) {
//case '\'':
//case '`':
//stream << FootBegin
//last_foot_begin = static_cast<long>(stream.tellp()) - 2
//last_phoneme[0] = rune('\x00')
//last_phoneme_ptr = last_phoneme
//break
//case '"':
//stream << SecondaryStress
//last_phoneme[0] = rune('\x00')
//last_phoneme_ptr = last_phoneme
//break
//case '_':
//case '.':
//stream << *ptr
//last_phoneme[0] = rune('\x00')
//last_phoneme_ptr = last_phoneme
//break
//case ' ':
//// SUPPRESS UNNECESSARY BLANKS
//if (*(ptr+1) && (*(ptr+1) != ' ')) {
//stream << *ptr
//last_phoneme[0] = rune('\x00')
//last_phoneme_ptr = last_phoneme
//}
//break
//default:
//stream << *ptr
//*last_phoneme_ptr++ = *ptr
//*last_phoneme_ptr = rune('\x00')
//break
//}
//ptr++
//}
//
//// ADD APPROPRIATE ENDING TO PRONUNCIATION IF POSSESSIVE
//if (possessive) {
//if (!strcmp(last_phoneme,"p") || !strcmp(last_phoneme,"t") ||
//!strcmp(last_phoneme,"k") || !strcmp(last_phoneme,"f") ||
//!strcmp(last_phoneme,"th")) {
//stream << "_s"
//} else if (!strcmp(last_phoneme,"s") || !strcmp(last_phoneme,"sh") ||
//!strcmp(last_phoneme,"z") || !strcmp(last_phoneme,"zh") ||
//!strcmp(last_phoneme,"j") || !strcmp(last_phoneme,"ch")) {
//stream << ".uh_z"
//} else {
//stream << "_z"
//}
//}
//
//// ADD SPACE AFTER WORD
//stream << ' '
//
//// IF TONIC, CONVERT LAST FOOT MARKER TO TONIC MARKER
//if (is_tonic && (last_foot_begin != UndefinedPosition)) {
//long temporaryPosition = static_cast<long>(stream.tellp())
//stream.seekp(last_foot_begin)
//stream << TonicBegin
//stream.seekp(temporaryPosition)
//}
