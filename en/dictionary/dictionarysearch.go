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
 *  but WITHOUT ANY WARRANTY without even the implied warranty of         *
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

package dictionary

type DictionarySearch struct {
	Dictionary *Dictionary
	buf        []rune
	typeBuf    []rune
}

func NewDictionarySearch() *DictionarySearch {
	ds := new(DictionarySearch)
	d := new(Dictionary)
	ds.Dictionary = d

	ds.buf = make([]rune, 1024)
	ds.typeBuf = make([]rune, 32)
	return ds
}

// Load
func (ds *DictionarySearch) Load(path string) {
	ds.Dictionary.Load(path)
}

// GetEntry returns the
func (ds *DictionarySearch) GetEntry(word string) string {
	phonetic := ds.AugmentedSearch(word)
	return phonetic
}

// Version returns the dictionary version
func (ds *DictionarySearch) Version() string {
	return ds.Dictionary.Version()
}

// AugmentedSearch first looks in main dictionary to see if word is there. If not,
// it tries the main dictionary without suffixes, and if found, tacks on the appropriate ending.
func (ds *DictionarySearch) AugmentedSearch(orthography string) string {
	word := ""
	//pt := ""
	//pos := -1

	//const suffix_list_t* list_ptr;

	word = ds.Dictionary.GetEntry(orthography)
	if len(word) > 0 {
		return word
	}

	ds.buf = ds.buf[:0]
	ds.typeBuf = ds.typeBuf[:0]

	// Todo: port suffix.go first!
	// loop through suffix list
	//for (list_ptr = suffix_list; list_ptr->suffix; list_ptr++) {
	//	// Todo: use strings.LastIndex to get position of substring (suffix)
	//	if ( (pt = word_has_suffix(orthography, list_ptr->suffix)) ) {
	//		/*  TACK ON REPLACEMENT ENDING  */
	//		strcpy(&buffer_[0], orthography);
	//		*(&buffer_[0] + (pt - orthography)) = '\0';
	//		strcat(&buffer_[0], list_ptr->replacement);
	//
	//		/*  IF WORD FOUND WITH REPLACEMENT ENDING  */
	//		if ( (word = dict_.getEntry(&buffer_[0])) ) {
	//			/*  PUT THE FOUND PRONUNCIATION IN THE BUFFER  */
	//			strcpy(&buffer_[0], word);
	//
	//			/*  FIND THE WORD-TYPE INFO  */
	//			for (word_type_pos = &buffer_[0]; *word_type_pos && (*word_type_pos != '%'); word_type_pos++)
	//				;
	//
	//			/*  SAVE IT INTO WORD TYPE BUFFER  */
	//			strcpy(&wordTypeBuffer_[0], word_type_pos);
	//
	//			/*  APPEND SUFFIX PRONUNCIATION TO WORD  */
	//			*word_type_pos = '\0';
	//			strcat(&buffer_[0], list_ptr->pronunciation);
	//
	//			/*  AND PUT BACK THE WORD TYPE  */
	//			strcat(&buffer_[0], &wordTypeBuffer_[0]);
	//
	//			/*  RETURN WORD WITH SUFFIX AND ORIGINAL WORD TYPE  */
	//			return &buffer_[0];
	//		}
	//	}
	//}

	return ""
}
