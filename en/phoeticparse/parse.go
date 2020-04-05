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

	v2 "github.com/emer/gospeech/v2"
)

const vowelTransitionsConfig = "/vowelTransitions"

type PhoneticParser struct {
	Model      *v2.Model
	Sequence   *v2.Sequence
	Categories []v2.Category
	ReturnPhone []v2.Posture
	VowelTransitions [][]int
}

func NewPhoneticParser(mdl *v2.Model, c *v2.Control, configPath string) *PhoneticParser {
	pp := PhoneticParser{}
	pp.Model = mdl
	pp.Sequence = c.Sequence
	pp.Categories = make([]v2.Category, 18) // why 18?
	pp.ReturnPhone = make([]v2.Posture, 7)  // why 7?
	pp.VowelTransitions = make([][]int, 13)

	pp.Categories[0] = *pp.Model.CategoryTry("stopped");
	pp.Categories[1] = *pp.Model.CategoryTry("affricate");
	pp.Categories[2] = *pp.Model.CategoryTry("hlike");
	pp.Categories[3] = *pp.Model.CategoryTry("vocoid");

	postureList := []string{"h", "h'", "hv", "hv'", "ll", "ll'", "s", "s'", "z", "z'"}
	for i, pname := range postureList {
		tp := pp.Model.PostureTry(pname)
		if tp == nil {
			log.Println("NewPhoneticParser: posture not found")
			return nil
		}
		tc := tp.CategoryTry(tp.Name) //	pp.Categories[i + 4U] = tempPosture->findCategory(posture);
		if tc == nil {
			log.Println("NewPhoneticParser: category not found")
			return nil
		}
		pp.Categories[i] = *tc //	pp.Categories[i + 4U] = tempPosture->findCategory(posture);

	}

	pp.Categories[14] = *pp.Model.CategoryTry("whistlehack");
	pp.Categories[15] = *pp.Model.CategoryTry("lhack");
	pp.Categories[16] = *pp.Model.CategoryTry("whistlehack");
	pp.Categories[17] = *pp.Model.CategoryTry("whistlehack");

	pp.ReturnPhone[0] = *pp.Model.PostureTry("qc");
	pp.ReturnPhone[1] = *pp.Model.PostureTry("qt");
	pp.ReturnPhone[2] = *pp.Model.PostureTry("qp");
	pp.ReturnPhone[3] = *pp.Model.PostureTry("qk");
	pp.ReturnPhone[4] = *pp.Model.PostureTry("gs");
	pp.ReturnPhone[5] = *pp.Model.PostureTry("qs");
	pp.ReturnPhone[6] = *pp.Model.PostureTry("qz");

	pp.InitVowelTransitions(configPath);

	return &pp
}

std::shared_ptr<TRMControlModel::Category>
func (pp *PhoneticParser) pp.Model.CategoryTry(const char* name)
{
	const std::shared_ptr<TRMControlModel::Category> category = model_.findCategory(name);
	if (!category) {
		THROW_EXCEPTION(UnavailableResourceException, "Could not find the category \"" << name << "\".");
	}
	return category;
}

const TRMControlModel::Posture*
func (pp *PhoneticParser) *pp.Model.PostureTry(const char* name)
{
	const TRMControlModel::Posture* posture = model_.postureList().find(name);
	if (!posture) {
		THROW_EXCEPTION(UnavailableResourceException, "Could not find the posture \"" << name << "\".");
	}
	return posture;
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
			pp.VowelTransitions[i][0], pp.VowelTransitions[i][1],  pp.VowelTransitions[i][2],
			pp.VowelTransitions[i][3], pp.VowelTransitions[i][4],  pp.VowelTransitions[i][5],
			pp.VowelTransitions[i][6], pp.VowelTransitions[i][7],  pp.VowelTransitions[i][8],
			pp.VowelTransitions[i][9], pp.VowelTransitions[i][10], pp.VowelTransitions[i][11],
			pp.VowelTransitions[i][12])
		i++
	}
	fp.Close()
}

void
func (pp *PhoneticParser) printVowelTransitions()
{
	printf("===== Transitions configuration:\n");
	for (int i = 0; i < 13; i++) {
		printf("Transition %d: %d %d %d %d %d %d %d %d %d %d %d %d %d\n", i,
				vowelTransitions_[i][0], vowelTransitions_[i][1], vowelTransitions_[i][2],
				vowelTransitions_[i][3], vowelTransitions_[i][4], vowelTransitions_[i][5],
				vowelTransitions_[i][6], vowelTransitions_[i][7], vowelTransitions_[i][8],
				vowelTransitions_[i][9], vowelTransitions_[i][10], vowelTransitions_[i][11],
				vowelTransitions_[i][12]);
	}
}

const TRMControlModel::Posture*
func (pp *PhoneticParser) rewrite(const TRMControlModel::Posture& nextPosture, int wordMarker, RewriterData& data)
{
	const TRMControlModel::Posture* tempPosture;
	int transitionMade = 0;
	const char* temp;
	const TRMControlModel::Posture* returnValue = nullptr;

	static const int stateTable[19][18] = {
		{ 1,  9,  0,  7,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 0 */
		{ 3,  9,  0,  7,  2,  2,  2,  2,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 1 */
		{ 1,  9,  0,  7,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 2 */
		{ 4,  9,  0,  7,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 3 */
		{ 1,  9,  0,  7,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 4 */
		{ 1,  9,  0,  6,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 5 */
		{ 1,  9,  0,  8,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 6 */
		{ 1,  9,  0,  8,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 7 */
		{ 1,  9,  0,  8,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 8 */
		{10, 12, 12,  0,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 9 */
		{11, 11, 11,  0,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 10 */
		{ 1,  9,  0,  0,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 11 */
		{ 1,  9,  0,  0,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 12 */
		{ 1,  9,  0,  0,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15, 14,  0,  0, 17},		/* State 13 */
		{ 1,  9,  0,  0,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 14 */
		{ 1,  9,  0,  0,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15, 16,  0,  0, 17},		/* State 15 */
		{ 1,  9,  0,  0,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 16 */
		{ 1,  9,  0,  0,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0, 18, 17},		/* State 17 */
		{ 1,  9,  0,  0,  0,  0,  0,  0,  5,  5, 13, 13, 15, 15,  0,  0,  0, 17},		/* State 18 */
	};

	for (int i = 0; i < 18; i++) {
		if (nextPosture.isMemberOfCategory(*pp.Categories[i])) {
			//printf("Found %s %s state %d -> %d\n", nextPhone.name().c_str(), pp.Categories[i]->name.c_str(),
			//	data.currentState, stateTable[data.currentState][i]);
			data.currentState = stateTable[data.currentState][i];
			transitionMade = 1;
			break;
		}
	}
	if (transitionMade) {
		switch (data.currentState) {
			default:
			case 0:
			case 1:
			case 3:
			case 5:
			case 7:
			case 9:
				//printf("No rewrite\n");
				break;
			case 2:
			case 4:
			case 11:
				temp = data.lastPosture->name().c_str();
				switch (temp[0]) {
					case 'd':
					case 't': returnValue = pp.ReturnPhone[1];
						break;
					case 'p':
					case 'b': returnValue = pp.ReturnPhone[2];
						break;
					case 'k':
					case 'g': returnValue = pp.ReturnPhone[3];
						break;
				}
				break;
			case 6:
				if (strchr(nextPosture.name().c_str(), '\'')) {
					tempPosture = model_.postureList().find("l'");
				} else {
					tempPosture = model_.postureList().find("l");
				}

				eventList_.replaceCurrentPostureWith(*tempPosture);

				break;
			case 8:
				if (wordMarker) {
					returnValue = calcVowelTransition(nextPosture, data);
				}

				break;
			case 10:
				returnValue = pp.ReturnPhone[0];
				break;
			case 12:
				returnValue = pp.ReturnPhone[0];
				break;
			case 14:
				returnValue = pp.ReturnPhone[5];
				break;
			case 16:
				returnValue = pp.ReturnPhone[6];
				break;
			case 18:
				//printf("Case 18\n");
				if (!wordMarker) {
					break;
				}

				if (strchr(nextPosture.name().c_str(), '\'')) {
					tempPosture = model_.postureList().find("ll'");
				} else {
					tempPosture = model_.postureList().find("ll");
				}

				//printf("Replacing with ll\n");
				eventList_.replaceCurrentPostureWith(*tempPosture);

				break;
		}
		data.lastPosture = &nextPosture;
	} else {
		data.currentState = 0;
		data.lastPosture = nullptr;
	}
	return returnValue;
}

int
func (pp *PhoneticParser) parseString(const char* string)
{
	const TRMControlModel::Posture* tempPosture;
	const TRMControlModel::Posture* tempPosture1;
	int length;
	int index = 0, bufferIndex = 0;
	int chunk = 0;
	char buffer[128];
	int lastFoot = 0, markedFoot = 0, wordMarker = 0;
	double footTempo = 1.0;
	double ruleTempo = 1.0;
	double postureTempo = 1.0;
	RewriterData rewriterData;

	length = strlen(string);

	tempPosture = model_.postureList().find("^");
	eventList_.newPostureWithObject(*tempPosture);

	while (index < length) {
		while ((isspace(string[index]) || (string[index] == '_')) && (index<length)) index++;
		if (index > length) break;

		memset(buffer, 0, 128);
		bufferIndex = 0;

		switch (string[index]) {
		case '/': /* Handle "/" escape sequences */
			index++;
			switch(string[index]) {
			case '0': /* Tone group 0. Statement */
				index++;
				eventList_.setCurrentToneGroupType(TONE_GROUP_TYPE_STATEMENT);
				break;
			case '1': /* Tone group 1. Exclamation */
				index++;
				eventList_.setCurrentToneGroupType(TONE_GROUP_TYPE_EXCLAMATION);
				break;
			case '2': /* Tone group 2. Question */
				index++;
				eventList_.setCurrentToneGroupType(TONE_GROUP_TYPE_QUESTION);
				break;
			case '3': /* Tone group 3. Continuation */
				index++;
				eventList_.setCurrentToneGroupType(TONE_GROUP_TYPE_CONTINUATION);
				break;
			case '4': /* Tone group 4. Semi-colon */
				index++;
				eventList_.setCurrentToneGroupType(TONE_GROUP_TYPE_SEMICOLON);
				break;
			case ' ':
			case '_': /* New foot */
				eventList_.newFoot();
				if (lastFoot) {
					eventList_.setCurrentFootLast();
				}
				footTempo = 1.0;
				lastFoot = 0;
				markedFoot = 0;
				index++;
				break;
			case '*': /* New Marked foot */
				eventList_.newFoot();
				eventList_.setCurrentFootMarked();
				if (lastFoot) {
					eventList_.setCurrentFootLast();
				}
				footTempo = 1.0;
				lastFoot = 0;
				markedFoot = 1;
				index++;
				break;
			case '/': /* New Tone Group */
				index++;
				eventList_.newToneGroup();
				break;
			case 'c': /* New Chunk */
				if (chunk) {
					tempPosture = model_.postureList().find("#");
					eventList_.newPostureWithObject(*tempPosture);
					tempPosture = model_.postureList().find("^");
					eventList_.newPostureWithObject(*tempPosture);
					index--;
					return index;
				} else {
					chunk++;
					index++;
				}
				break;
			case 'l': /* Last Foot in tone group marker */
				index++;
				lastFoot = 1;
				break;
			case 'w': /* word marker */
				index++;
				wordMarker = 1;
				break;
			case 'f': /* Foot tempo indicator */
				index++;
				while ((isspace(string[index]) || (string[index] == '_')) && (index < length)) {
					index++;
				}
				if (index > length) {
					break;
				}
				while (isdigit(string[index]) || (string[index] == '.')) {
					buffer[bufferIndex++] = string[index++];
				}
				footTempo = atof(buffer);
				eventList_.setCurrentFootTempo(footTempo);
				break;
			case 'r': /* Foot tempo indicator */
				index++;
				while ((isspace(string[index]) || (string[index] == '_')) && (index < length)) {
					index++;
				}
				if (index > length) {
					break;
				}
				while (isdigit(string[index]) || (string[index] == '.')) {
					buffer[bufferIndex++] = string[index++];
				}
				ruleTempo = atof(buffer);
				break;
			default:
				index++;
				//printf("Unknown \"/\" escape sequence :%c\n", string[index]);
				break;
			}
			break;
		case '.': /* Syllable Marker */
			eventList_.setCurrentPostureSyllable();
			index++;
			break;

		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			while (isdigit(string[index]) || (string[index] == '.')) {
				buffer[bufferIndex++] = string[index++];
			}
			postureTempo = atof(buffer);
			break;

		default:
			if (isalpha(string[index]) || (string[index] == '^') || (string[index] == '\'')
					|| (string[index] == '#') ) {
				while ( (isalpha(string[index])||(string[index] == '^')||(string[index] == '\'')
						||(string[index] == '#')) && (index < length)) {
					buffer[bufferIndex++] = string[index++];
				}
				if (markedFoot) {
					strcat(buffer,"'");
				}
				tempPosture = model_.postureList().find(buffer);
				if (tempPosture) {
					tempPosture1 = rewrite(*tempPosture, wordMarker, rewriterData);
					if (tempPosture1) {
						eventList_.newPostureWithObject(*tempPosture1);
					}
					eventList_.newPostureWithObject(*tempPosture);
					eventList_.setCurrentPostureTempo(postureTempo);
					eventList_.setCurrentPostureRuleTempo((float) ruleTempo);
				}
				postureTempo = 1.0;
				ruleTempo = 1.0;
				wordMarker = 0;
			} else {
				//printf("Unknown character %c\n", string[index++]);
				break;
			}
		}
	}
	return 0;
}

const TRMControlModel::Posture*
func (pp *PhoneticParser) calcVowelTransition(const TRMControlModel::Posture& nextPosture, RewriterData& data)
{
	int vowelHash[13] = { 194, 201, 97, 101, 105, 111, 221, 117, 211, 216, 202, 215, 234 };
	int lastValue, nextValue, i, action;
	const char* temp;

	temp = data.lastPosture->name().c_str();
	lastValue = (int) temp[0];
	if (temp[1] != '\'') {
		lastValue += (int) temp[1];
	}

	for (i = 0; i < 13; i++) {
		if (lastValue == vowelHash[i]) {
			lastValue = i;
			break;
		}
	}
	if (i == 13) {
		return nullptr;
	}

	temp = nextPosture.name().c_str();
	nextValue = (int) temp[0];
	if (temp[1] != '\'') {
		nextValue += (int) temp[1];
	}

	for (i = 0; i < 13; i++) {
		if (nextValue == vowelHash[i]) {
			nextValue = i;
			break;
		}
	}
	if (i == 13) {
		return nullptr;
	}

	action = vowelTransitions_[lastValue][nextValue];

	switch (action) {
	default:
	case 0:
		return nullptr;
	case 1: return model_.postureList().find("gs");
	case 2: return model_.postureList().find("r");
	}
}
