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

package v2

import (
	"log"
)

type Target struct {
	Name  string  `xml:"name,attr"`
	Value float64 `xml:"value,attr"`
}

type Posture struct {
	Name         string     `xml:"symbol,attr"`
	Categories   []Category `xml:"posture-categories>category-ref"`
	ParamTargets []Target   `xml:"parameter-targets>target"`
	SymTargets   []Target   `xml:"symbol-targets>target"`
	Comment      string     `xml:"comment"`
}

func NewPosture(nm string, paramN, symN int) *Posture {
	np := Posture{}
	np.Name = nm
	if paramN == 0 || symN == 0 {
		log.Println("paramN and symN must be > 0")
		return nil
	}
	np.ParamTargets = make([]Target, paramN)
	np.SymTargets = make([]Target, symN)

	var cat Category
	cat.Name = nm
	cat.Native = true
	np.Categories = append(np.Categories, cat)
	return &np
}

func (pos *Posture) Copy(newNm string) *Posture {
	np := NewPosture(newNm, len(pos.ParamTargets), len(pos.SymTargets))
	for _, c := range pos.Categories {
		if !c.Native {
			np.Categories = append(np.Categories, c)
		}
	}
	np.Comment = pos.Comment
	return np
}

func (pos *Posture) IsMemberOfCategory(category *Category) bool {
	for _, c := range pos.Categories {
		if &c == category {
			return true
		}
	}
	return false
}

// CategoryTry returns the address of the named Posture or nil if not found
func (pos *Posture) CategoryTry(nm string) *Category {
	for _, c := range pos.Categories {
		if c.Name == nm {
			return &c
		}
	}
	return nil
}
