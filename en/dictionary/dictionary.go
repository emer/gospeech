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

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

type Dictionary struct {
	version string
	Entries map[string]string
}

// Load
func (d *Dictionary) Load(path string) {
	d.Entries = make(map[string]string)

	fp, err := os.Open(path) // For read access.
	if err != nil {
		log.Fatal(err)
	} else {
		fmt.Println(d.Version())
	}
	defer fp.Close() // we will be done with the file within this function

	scanner := bufio.NewScanner(fp)
	scanner.Split(bufio.ScanLines)
	var msg string
	for scanner.Scan() {
		s := scanner.Text()
		fields := strings.Fields(s)
		if len(fields) == 0 { // possible blank line
			continue
		}
		if len(fields) != 2 {
			msg = "Dictionary Load(): the file " + path + "  does not have a key value entry pair on each line"
			log.Println(msg)
		}
		// Todo: do we need to check of duplicate keys?
		d.Entries[fields[0]] = fields[1]
	}
}

// GetEntry
func (d *Dictionary) GetEntry(word string) string {
	for k, v := range d.Entries {
		if k == word {
			return v
		}
	}
	return ""
}

func (d *Dictionary) Version() string {
	if len(d.Entries) == 0 {
		return ""
	}
	return d.version
}
