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

package v2

import (
	"encoding/xml"
	"errors"
	"io/ioutil"
)

type Model struct {
	Categories     []Category `xml:"categories>category"`
	Params         []Param    `xml:"parameters>parameter"`
	Symbols        []Symbol   `xml:"symbols>symbol"`
	Postures       []Posture  `xml:"postures>posture"`
	Rules          []Rule     `xml:"rules>rule"`
	EqGrps         []EqGrp    `xml:"equations>equation-group"`
	TransGrps      []TransGrp `xml:"transitions>transition-group"`
	TransGrpsSp    []TransGrp `xml:"special-transitions>transition-group"`
	FormulaSymbols *FormulaSymbols
}

func NewModel(fullpath string) *Model {
	m := Model{}
	m.Categories = make([]Category, 0)
	m.Params = make([]Param, 0)
	m.Symbols = make([]Symbol, 0)
	m.Postures = make([]Posture, 0)
	m.Rules = make([]Rule, 0)
	m.EqGrps = make([]EqGrp, 0)
	m.TransGrps = make([]TransGrp, 0)
	m.TransGrpsSp = make([]TransGrp, 0)
	m.Load(fullpath) // load config file -- monet_go.xml - our own version - ToDo: add notes to readme
	return &m
}

// Reset
func (mdl *Model) Reset() {
	mdl.Categories = mdl.Categories[:0]
	mdl.Params = mdl.Params[:0]
	mdl.Symbols = mdl.Symbols[:0]
	mdl.Postures = mdl.Postures[:0]
	mdl.Rules = mdl.Rules[:0]
	mdl.EqGrps = mdl.EqGrps[:0]
	mdl.TransGrps = mdl.TransGrps[:0]
	mdl.TransGrpsSp = mdl.TransGrpsSp[:0]
	mdl.FormulaSymbols.Clear()
}

// Load the model configuration - the monet.xml file (postures, intonation, etc)
func (mdl *Model) Load(path string) {
	//Reset()
	data, err := ioutil.ReadFile(path)

	err = xml.Unmarshal([]byte(data), &mdl)
	if err != nil {
		panic(err)
	}

	// Some clean up after unmarshalling
	// for enums we read in as a string and then set the enum value
	// I could write the xml unmarshalling to handle this but I don't think there are
	// enough cases to warrant
	for _, tg := range mdl.TransGrps {
		for _, t := range tg.Transitions {
			switch t.TypeAsStr {
			case "diphone":
				t.Type = TransDiPhone
			case "triphone":
				t.Type = TransTriPhone
			case "tetraphone":
				t.Type = TransTetraPhone
			default:
				t.Type = TransInvalid
			}
		}
	}

	//fmt.Printf(mdl.Categories[0].Name)

	// LOG_DEBUG("Loading xml configuration: " << fp)
	// XMLConfigFileReader cfg(*this, fp);
	// cfg.loadModel();
}

// Save
func (mdl *Model) Save(configDir, configFile string) {
	//fp := configDir + configFile;
	//
	//// LOG_DEBUG("Saving xml configuration: " << fp);
	//// XMLConfigFileWriter cfg(*this, fp);
	//cfg.saveModel();
}

// ParamIdx returns the index or -1 if not found
func (mdl *Model) ParamIdx(nm string) int {
	for i, p := range mdl.Params {
		if p.Name == nm {
			return i
		}
	}
	return -1
}

//CategoryTry returns the address of the named Category or nil if not found
func (mdl *Model) CategoryTry(nm string) *Category {
	for _, c := range mdl.Categories {
		if c.Name == nm {
			return &c
		}
	}
	return nil
}

// CategoryTry returns the address of the named Posture or nil if not found
func (mdl *Model) PostureTry(nm string) *Posture {
	for _, p := range mdl.Postures {
		if p.Name == nm {
			return &p
		}
	}
	return nil
}

// EvalEquationFormula
func (mdl *Model) EvalEquationFormul(eq *Equation) float64 {

	return eq.Eval(mdl.FormulaSymbols)
}

// FindEquationGroup
func (mdl *Model) findEquationGroupTry(nm string) *EqGrp {
	for _, eg := range mdl.EqGrps {
		if eg.Name == nm {
			return &eg
		}
	}
	return nil
}

// EquationIndexTry returns the group and equation index if name is found, otherwise -1, -1
func (mdl *Model) EquationIndexTry(nm string) (grpIdx, eqIdx int) {
	for i, grp := range mdl.EqGrps {
		for j, eq := range grp.Equations {
			if eq.Name == nm {
				return i, j
			}
		}
	}
	return -1, -1
}

// EquationTry
func (mdl *Model) EquationTry(nm string) *Equation {
	for _, grp := range mdl.EqGrps {
		for _, eq := range grp.Equations {
			if eq.Name == nm {
				return &eq
			}
		}
	}
	return nil
}

// ParamMin returns the minimum parameter value
func (mdl *Model) ParamMin(idx int) (float64, error) {
	if idx >= len(mdl.Params) {
		return 0.0, errors.New("Parameter index out of range")
	}
	return mdl.Params[idx].Min, nil
}

// ParamMax returns the minimum parameter value
func (mdl *Model) ParamMax(idx int) (float64, error) {
	if idx >= len(mdl.Params) {
		return 0.0, errors.New("Parameter index out of range")
	}
	return mdl.Params[idx].Max, nil
}

// Param returns the parameter at given index
func (mdl *Model) Param(idx int) (*Param, error) {
	if idx >= len(mdl.Params) {
		return nil, errors.New("Parameter index out of range")
	}
	return &mdl.Params[idx], nil
}

// ParamTry returns the address of the named Param or nil if not found
func (mdl *Model) ParamTry(nm string) *Param {
	for _, p := range mdl.Params {
		if p.Name == nm {
			return &p
		}
	}
	return nil
}

// SymbolTry returns the address of the named Symbol or nil if not found
func (mdl *Model) SymbolTry(nm string) *Symbol {
	for _, s := range mdl.Symbols {
		if s.Name == nm {
			return &s
		}
	}
	return nil
}

// TransitionTry returns the address of the named Transition or nil if not found
func (mdl *Model) TransitionTry(nm string) *Transition {
	for _, grp := range mdl.TransGrps {
		for _, tr := range grp.Transitions {
			if tr.Name == nm {
				return &tr
			}
		}
	}
	return nil
}

// TransitionGrpTry returns the address of the named TransitionGroup or nil if not found
func (mdl *Model) TransitionGroupTry(nm string) *TransGrp {
	for _, grp := range mdl.TransGrps {
		if grp.Name == nm {
			return &grp
		}
	}
	return nil
}

// TransitionGroupIndexTry returns the group and transition index if name is found, otherwise -1, -1
func (mdl *Model) TransitionGroupIndexTry(nm string) (grpIdx, eqIdx int) {
	for i, grp := range mdl.TransGrps {
		for j, tr := range grp.Transitions {
			if tr.Name == nm {
				return i, j
			}
		}
	}
	return -1, -1
}

// TransitionSpTry returns the address of the named special Transition or nil if not found
func (mdl *Model) TransitionSpTry(nm string) *Transition {
	for _, grp := range mdl.TransGrpsSp {
		for _, tr := range grp.Transitions {
			if tr.Name == nm {
				return &tr
			}
		}
	}
	return nil
}

// TransitionGroupSpTry returns the address of the named special TransitionGroup or nil if not found
func (mdl *Model) TransitionGroupSpTry(nm string) *TransGrp {
	for _, grp := range mdl.TransGrpsSp {
		if grp.Name == nm {
			return &grp
		}
	}
	return nil
}

// TransitionGroupSpIndexTry returns the special group and transition index if name is found, otherwise -1, -1
func (mdl *Model) TransitionGroupSpIndexTry(nm string) (grpIdx, eqIdx int) {
	for i, grp := range mdl.TransGrpsSp {
		for j, tr := range grp.Transitions {
			if tr.Name == nm {
				return i, j
			}
		}
	}
	return -1, -1
}

// FirstRule finds the first Rule that matches the given sequence of Postures. Returns rule and index
func (mdl *Model) FirstRule(postureSequence []Posture, ruleIdx int) (*Rule, int) {
	if len(mdl.Rules) == 0 {
		return nil, -1
	}
	for i, r := range mdl.Rules {
		if len(r.BoolExprs) <= len(postureSequence) {
			if r.EvalRuleExpr(postureSequence) {
				return &r, i
			}
		}
	}
	return nil, 0
}
