// Copyright (c) 2020, The Emergent Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

/***************************************************************************
 *  Copyright 1991, 1992, 1993, 1994, 1995, 1996, 2001, 2002               *
 *    David R. Hill, Leonard Manzara, Craig Schock                         *
 *                                                                         *
 *  This program is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by   *
 *  the Free Software Foundation, either version 3 of the License, or      *
 *  (at your option) any later version.                                    *
 *                                                                         *
 *  This program is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of         *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *
 *  GNU General Public License for more details.                           *
 *                                                                         *
 *  You should have received a copy of the GNU General Public License      *
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.  *
 ***************************************************************************/
// 2014-09
// This file was copied from Gnuspeech and modified by Marcelo Y. Matuda.

// 2019-02
// This is a port to golang of the C++ Gnuspeech port by Marcelo Y. Matuda

package phoneticparse

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
	"unicode"

	"github.com/emer/gospeech/v2"
)

const vowelTransitionsConfig = "/vowelTransitions"

type RewriterData struct {
	curState    int
	lastPosture *v2.Posture
}

type PhoneticParser struct {
	Model            *v2.Model
	Sequence         *v2.Sequence
	Categories       []*v2.Category
	ReturnPhone      []*v2.Posture
	VowelTransitions [][]int
}

func NewPhoneticParser(mdl *v2.Model, c *v2.Control, configPath string) *PhoneticParser {
	pp := PhoneticParser{}
	pp.Model = mdl
	pp.Sequence = c.Sequence
	pp.Categories = make([]*v2.Category, 18) // why 18?
	pp.ReturnPhone = make([]*v2.Posture, 7)  // why 7?
	pp.VowelTransitions = make([][]int, 13)

	foo := pp.Model.CategoryTry("stopped")
	if foo == nil {

	}
	pp.Categories[0] = pp.Model.CategoryTry("stopped")
	pp.Categories[1] = pp.Model.CategoryTry("affricate")
	pp.Categories[2] = pp.Model.CategoryTry("hlike")
	pp.Categories[3] = pp.Model.CategoryTry("vocoid")

	postureList := []string{"h", "h'", "hv", "hv'", "ll", "ll'", "s", "s'", "z", "z'"}
	for i, pname := range postureList {
		tp := pp.Model.PostureTry(pname)
		if tp == nil {
			log.Println("NewPhoneticParser: posture not found")
			return nil
		}
		tc := tp.CategoryTry(tp.Name) //	pp.Categories[i + 4U] = tp->findCategory(posture);
		if tc == nil {
			log.Println("NewPhoneticParser: category not found")
			return nil
		}
		pp.Categories[i] = tc //	pp.Categories[i + 4U] = tp->findCategory(posture);

	}

	pp.Categories[14] = pp.Model.CategoryTry("whistlehack")
	pp.Categories[15] = pp.Model.CategoryTry("lhack")
	pp.Categories[16] = pp.Model.CategoryTry("whistlehack")
	pp.Categories[17] = pp.Model.CategoryTry("whistlehack")

	pp.ReturnPhone[0] = pp.Model.PostureTry("qc")
	pp.ReturnPhone[1] = pp.Model.PostureTry("qt")
	pp.ReturnPhone[2] = pp.Model.PostureTry("qp")
	pp.ReturnPhone[3] = pp.Model.PostureTry("qk")
	pp.ReturnPhone[4] = pp.Model.PostureTry("gs")
	pp.ReturnPhone[5] = pp.Model.PostureTry("qs")
	pp.ReturnPhone[6] = pp.Model.PostureTry("qz")

	pp.InitVowelTransitions(configPath)

	return &pp
}

func (pp *PhoneticParser) InitVowelTransitions(configPath string) {
	fp, err := os.Open(configPath + vowelTransitionsConfig)
	if err != nil {
		log.Println("Could not open the file " + configPath)
		return
	}
	f := bufio.NewReader(fp)
	i := 0
	for {
		line, isP, err := f.ReadLine()
		if err == io.EOF {
			break
		}
		if isP == true {
			log.Println("ParseGroups: partial line read, will likely be a problem")
		}
		if i == 13 {
			break
		}
		if line[0] == '#' || line[0] == ' ' {
			// skip
		} else {

		}
		fmt.Sscanf(string(line), "%s %d %d %d %d %d %d %d %d %d %d %d %d %d",
			pp.VowelTransitions[i][0], pp.VowelTransitions[i][1], pp.VowelTransitions[i][2],
			pp.VowelTransitions[i][3], pp.VowelTransitions[i][4], pp.VowelTransitions[i][5],
			pp.VowelTransitions[i][6], pp.VowelTransitions[i][7], pp.VowelTransitions[i][8],
			pp.VowelTransitions[i][9], pp.VowelTransitions[i][10], pp.VowelTransitions[i][11],
			pp.VowelTransitions[i][12])
		i++
	}
	fp.Close()
}

//func (pp *PhoneticParser) printVowelTransitions()
//{
//	printf("===== Transitions configuration:\n");
//	for (int i = 0; i < 13; i++) {
//		printf("Transition %d: %d %d %d %d %d %d %d %d %d %d %d %d %d\n", i,
//				vowelTransitions_[i][0], vowelTransitions_[i][1], vowelTransitions_[i][2],
//				vowelTransitions_[i][3], vowelTransitions_[i][4], vowelTransitions_[i][5],
//				vowelTransitions_[i][6], vowelTransitions_[i][7], vowelTransitions_[i][8],
//				vowelTransitions_[i][9], vowelTransitions_[i][10], vowelTransitions_[i][11],
//				vowelTransitions_[i][12]);
//	}
//}

func (pp *PhoneticParser) Rewrite(nextPosture *v2.Posture, wordMarker bool, data RewriterData) (rv *v2.Posture) {

	var stateTable = [19][18]int{
		{1, 9, 0, 7, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 0 */
		{3, 9, 0, 7, 2, 2, 2, 2, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 1 */
		{1, 9, 0, 7, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 2 */
		{4, 9, 0, 7, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 3 */
		{1, 9, 0, 7, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 4 */
		{1, 9, 0, 6, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 5 */
		{1, 9, 0, 8, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 6 */
		{1, 9, 0, 8, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 7 */
		{1, 9, 0, 8, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 8 */
		{10, 12, 12, 0, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17}, /* State 9 */
		{11, 11, 11, 0, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17}, /* State 10 */
		{1, 9, 0, 0, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 11 */
		{1, 9, 0, 0, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 12 */
		{1, 9, 0, 0, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 14, 0, 0, 17},   /* State 13 */
		{1, 9, 0, 0, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 14 */
		{1, 9, 0, 0, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 16, 0, 0, 17},   /* State 15 */
		{1, 9, 0, 0, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 16 */
		{1, 9, 0, 0, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 18, 17},   /* State 17 */
		{1, 9, 0, 0, 0, 0, 0, 0, 5, 5, 13, 13, 15, 15, 0, 0, 0, 17},    /* State 18 */
	}

	transitionMade := false
	for i := 0; i < 18; i++ {
		c := pp.Categories[i]
		if nextPosture.IsMemberOfCategory(c) {
			//printf("Found %s %s state %d -> %d\n", nextPhone.name().c_str(), pp.Categories[i]->name.c_str(), //
			//	data.currentState, stateTable[data.currentState][i]);
			data.curState = stateTable[data.curState][i]
			transitionMade = true
			break
		}
	}
	if transitionMade {
		switch data.curState {
		default:
		case 0:
		case 1:
		case 3:
		case 5:
		case 7:
		case 9:
			//printf("No rewrite\n");
			break
		case 2:
		case 4:
		case 11:
			temp := data.lastPosture.Name
			switch temp[0] {
			case 'd':
			case 't':
				rv = pp.ReturnPhone[1]
				break
			case 'p':
			case 'b':
				rv = pp.ReturnPhone[2]
				break
			case 'k':
			case 'g':
				rv = pp.ReturnPhone[3]
				break
			}
			break
		case 6:
			var tp *v2.Posture
			if strings.Contains(nextPosture.Name, "'") {
				tp = pp.Model.PostureTry("l'")
			} else {
				tp = pp.Model.PostureTry("l")
			}
			pp.Sequence.PostureDatum[pp.Sequence.CurPosture].Posture = tp
		case 8:
			if wordMarker {
				rv = pp.CalcVowelTransition(nextPosture, &data)
			}
		case 10:
			rv = pp.ReturnPhone[0]
		case 12:
			rv = pp.ReturnPhone[0]
		case 14:
			rv = pp.ReturnPhone[5]
		case 16:
			rv = pp.ReturnPhone[6]
		case 18:
			//printf("Case 18\n");
			if !wordMarker {
				break
			}
			var tp *v2.Posture
			if strings.Contains(nextPosture.Name, "'") {
				tp = pp.Model.PostureTry("lll'")
			} else {
				tp = pp.Model.PostureTry("l")
			}
			//printf("Replacing with ll\n");
			pp.Sequence.PostureDatum[pp.Sequence.CurPosture].Posture = tp
		}
		data.lastPosture = nextPosture
	} else {
		data.curState = 0
		data.lastPosture = nil
	}
	return rv
}

func (pp *PhoneticParser) ParseString(str string) int {
	var tp1 *v2.Posture
	var tp2 *v2.Posture

	buffer := ""
	bufidx := 0
	chunk := 0

	lastFoot := false
	markedFoot := false
	wordMarker := false

	footTempo := 1.0
	ruleTempo := 1.0
	postureTempo := 1.0

	var rewrite RewriterData

	length := len(str)

	tp1 = pp.Model.PostureTry("^")
	pp.Sequence.NewPostureWithObject(tp1)

	idx := 0
	for idx < length {
		for (unicode.IsSpace(rune(str[idx])) || rune(str[idx]) == '_') && (idx < length) {
			idx++
			if idx > length {
				break
			}
			bufidx = 0

			switch str[idx] {
			case '/': /* Handle "/" escape sequences */
				idx++
				switch rune(str[idx]) {
				case '0': /* Tone group 0. Statement */
					idx++
					pp.Sequence.CurToneGroup = v2.ToneStatement
				case '1': /* Tone group 1. Exclamation */
					idx++
					pp.Sequence.CurToneGroup = v2.ToneExclamation
				case '2': /* Tone group 2. Question */
					idx++
					pp.Sequence.CurToneGroup = v2.ToneQuestion
				case '3': /* Tone group 3. Continuation */
					idx++
					pp.Sequence.CurToneGroup = v2.ToneContinuation
				case '4': /* Tone group 4. Semi-colon */
					idx++
					pp.Sequence.CurToneGroup = v2.ToneSemicolon
				case ' ':
					fallthrough
				case '_': /* New foot */
					pp.Sequence.NewFoot()
					if lastFoot {
						pp.Sequence.Feet[len(pp.Sequence.Feet)-1].Last = true
					}
					footTempo = 1.0
					lastFoot = false
					markedFoot = false
					idx++
				case '*': /* New Marked foot */
					pp.Sequence.NewFoot()
					pp.Sequence.Feet[pp.Sequence.CurFoot].Marked = true
					if lastFoot {
						pp.Sequence.Feet[pp.Sequence.CurFoot].Last = true
					}
					footTempo = 1.0
					lastFoot = false
					markedFoot = true
					idx++
				case '/': /* New Tone Group */
					idx++
					pp.Sequence.NewToneGroup()
				case 'c': /* New Chunk */
					if chunk > 0 {
						tp1 = pp.Model.PostureTry("#")
						pp.Sequence.NewPostureWithObject(tp1)
						tp1 = pp.Model.PostureTry("^")
						pp.Sequence.NewPostureWithObject(tp1)
						idx--
						return idx
					} else {
						chunk++
						idx++
					}
				case 'l': /* Last Foot in tone group marker */
					idx++
					lastFoot = true
				case 'w': /* word marker */
					idx++
					wordMarker = true
				case 'f': /* Foot tempo indicator */
					idx++
					for (unicode.IsSpace(rune(str[idx])) || rune(str[idx]) == '_') && (idx < length) {
						idx++
					}
					if idx > length {
						break
					}
					for unicode.IsDigit(rune(str[idx])) || rune(str[idx]) == '.' {
						replaceAtIndex(buffer, str[idx], bufidx)
						bufidx++
						idx++
					}
					footTempo, _ = strconv.ParseFloat(buffer, 64)
					pp.Sequence.Feet[pp.Sequence.CurFoot].Tempo = footTempo
				case 'r': /* Foot tempo indicator */
					idx++
					for unicode.IsSpace(rune(str[idx])) || rune(str[idx]) == '_' && idx < length {
						idx++
					}
					if idx > length {
						break
					}
					for unicode.IsDigit(rune(str[idx])) || rune(str[idx]) == '.' {
						replaceAtIndex(buffer, str[idx], bufidx)
						bufidx++
						idx++
					}
					ruleTempo, _ = strconv.ParseFloat(buffer, 64)
				default:
					idx++
				}
				break
			case '.': /* Syllable Marker */
				pp.Sequence.PostureDatum[pp.Sequence.CurPosture].Syllable = true
				idx++
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
				for unicode.IsDigit(rune(str[idx])) || rune(str[idx]) == '.' {
					replaceAtIndex(buffer, str[idx], bufidx)
					bufidx++
					idx++
				}
				postureTempo, _ = strconv.ParseFloat(buffer, 64)
			default:
				if unicode.IsLetter(rune(str[idx])) || rune(str[idx]) == '^' || rune(str[idx]) == '\'' || rune(str[idx]) == '#' {
					for (unicode.IsLetter(rune(str[idx])) || rune(str[idx]) == '^' || rune(str[idx]) == '\'' || rune(str[idx]) == '#') && idx < length {
						replaceAtIndex(buffer, str[idx], bufidx)
						bufidx++
						idx++
					}
					if markedFoot {
						buffer = buffer + "'"
					}
					tp1 = pp.Model.PostureTry(buffer)
					if tp1 != nil {
						tp2 = pp.Rewrite(tp1, wordMarker, rewrite)
						if tp2 != nil {
							pp.Sequence.NewPostureWithObject(tp2)
						}
						pp.Sequence.NewPostureWithObject(tp1)
						pp.Sequence.PostureTempos[pp.Sequence.CurPosture] = postureTempo
						pp.Sequence.PostureDatum[pp.Sequence.CurPosture].RuleTempo = ruleTempo
					}
					postureTempo = 1.0
					ruleTempo = 1.0
					wordMarker = false
				} else {
					//printf("Unknown character %c\n", string[idx++]);
					break
				}
			}
		}
	}
	return 0
}

func replaceAtIndex(input string, replacement byte, index int) string {
	return strings.Join([]string{input[:index], string(replacement), input[index+1:]}, "")
}

// CalcVowelTransition
func (pp *PhoneticParser) CalcVowelTransition(nextPosture *v2.Posture, data *RewriterData) *v2.Posture {

	vowelHash := [13]int{194, 201, 97, 101, 105, 111, 221, 117, 211, 216, 202, 215, 234}
	lv := 0 // last value
	nv := 0 // next value
	i := 0
	action := 0
	temp := ""

	temp = data.lastPosture.Name
	lv = int(temp[0])
	if temp[1] != '\'' {
		lv += int(temp[1])
	}

	for i := 0; i < 13; i++ {
		if lv == vowelHash[i] {
			lv = i
			break
		}
	}
	if i == 13 {
		return nil
	}

	temp = nextPosture.Name
	nv = int(temp[0])
	if temp[1] != '\'' {
		nv += int(temp[1])
	}

	for i := 0; i < 13; i++ {
		if nv == vowelHash[i] {
			nv = i
			break
		}
	}
	if i == 13 {
		return nil
	}

	action = pp.VowelTransitions[lv][nv]

	switch action {
	default:
	case 0:
		return nil
	case 1:
		return pp.Model.PostureTry("gs")
	case 2:
		return pp.Model.PostureTry("r")
	}
	return nil
}
