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
	"log"
	"strconv"
)

type Model struct {
	Categories  []Category `xml:"categories>category"`
	Params      []Param    `xml:"parameters>parameter"`
	Symbols     []Symbol   `xml:"symbols>symbol"`
	Postures    []Posture  `xml:"postures>posture"`
	Rules       []*Rule    `xml:"rules>rule"`
	EqGrps      []EqGrp    `xml:"equations>equation-group"`
	TransGrps   []TransGrp `xml:"transitions>transition-group"`
	TransGrpsSp []TransGrp `xml:"special-transitions>transition-group"`
	FormulaVals FormulaValueList
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
	mdl.ClearFormulaVals()
}

func (mdl *Model) ClearFormulaVals() {
	for i, _ := range mdl.FormulaVals {
		mdl.FormulaVals[i] = 0
	}
}

func (grp *TransGrp) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	var tr *Transition
	for {
		t, err := d.Token()
		if err != nil {
			return err
		}
		grp.Name = start.Attr[0].Value // the only attribute

		switch tt := t.(type) {
		case xml.StartElement:
			switch tt.Name.Local {
			case "transition":
				tr = new(Transition)
				tr.PtSlpList = make([]interface{}, 0)
				grp.Transitions = append(grp.Transitions, tr)
				for _, attr := range tt.Attr {
					switch attr.Name.Local {
					case "name":
						tr.Name = attr.Value
					case "type":
						switch attr.Value {
						case "diphone":
							tr.Type = TransDiPhone
						case "triphone":
							tr.Type = TransTriPhone
						case "tetraphone":
							tr.Type = TransTetraPhone
						default:
							tr.Type = TransInvalid
						}
					}
				}
			case "point":
				p := new(Point)
				tr.PtSlpList = append(tr.PtSlpList, *p)
				for _, attr := range tt.Attr {
					switch attr.Name.Local {
					case "value":
						p.Value, err = strconv.ParseFloat(attr.Value, 64)
					case "type":
						switch attr.Value {
						case "diphone":
							p.Type = TransDiPhone
						case "triphone":
							p.Type = TransTriPhone
						case "tetraphone":
							p.Type = TransTetraPhone
						default:
							p.Type = TransInvalid
						}
					case "time-expression":
						e := new(Equation)
						p.TimeExpr = *e
						e.Name = attr.Value
					case "free-time":
						ft := attr.Value
						p.FreeTime, err = strconv.ParseFloat(ft, 64)
					case "is-phantom":
						if attr.Value == "yes" {
							p.IsPhantom = true
						}
					}
				}
			case "slope":
				s := new(Slope)
				tr.PtSlpList = append(tr.PtSlpList, *s)
				for _, attr := range tt.Attr {
					switch attr.Name.Local {
					case "slope":
						sl := attr.Value
						s.Slope, err = strconv.ParseFloat(sl, 64)
					case "display-time":
						dt := attr.Value
						s.DisplayTime, err = strconv.ParseFloat(dt, 64)
					}
				}
			}
		case xml.EndElement:
			if tt == start.End() {
				return nil
			}
		}
	}
}

func (r *Rule) UnmarshalXML(d *xml.Decoder, start xml.StartElement) error {
	// context -- either param-profiles or special-profiles -
	// needed a way to know which parameter-transition list to add to
	context := ""
	for {
		t, err := d.Token()
		if err != nil {
			return err
		}
		switch tt := t.(type) {
		case xml.StartElement:
			var s string
			switch tt.Name.Local {
			case "boolean-expression":
				d.DecodeElement(&s, &start)
				r.BoolExprs = append(r.BoolExprs, s)
			case "parameter-profiles":
				context = "param"
			case "special-profiles":
				context = "special"
			case "parameter-transition":
				pt := new(ParamTransition)
				if context == "param" {
					r.ParamTransitions = append(r.ParamTransitions, pt)
				} else {
					r.SpecialTransitions = append(r.SpecialTransitions, pt)
				}
				for _, attr := range tt.Attr {
					switch attr.Name.Local {
					case "transition":
						pt.Type = attr.Value
					case "name":
						pt.Name = attr.Value
					}
				}
				// after loading set pt.Transition to point to the transition
				pt.Transition = nil
			case "symbol-equation":
				se := new(ExprSymEquation)
				for _, attr := range tt.Attr {
					switch attr.Name.Local {
					case "name":
						se.SymName = attr.Value
					case "equation":
						se.EqName = attr.Value
					}
				}
				r.ExprSymEquations = append(r.ExprSymEquations, se)
			}

		case xml.EndElement:
			if tt == start.End() {
				return nil
			}
		}
	}
	return nil
}

// Load the model configuration - the monet.xml file (postures, intonation, etc)
func LoadModel(path string) *Model {
	data, err := ioutil.ReadFile(path)
	if err != nil {
		log.Println(err)
	}

	mdl := Model{}
	err = xml.Unmarshal([]byte(data), &mdl)
	if err != nil {
		panic(err)
	}

	// need to take bool expressions and build the list of bool nodes
	// could also do in custom unmarshall
	for i, _ := range mdl.Rules {
		mdl.Rules[i].BoolNodes = make([]BoolNode, 0)
		nodes := mdl.Rules[i].SetExprList(mdl.Rules[i].BoolExprs, &mdl)
		for _, n := range *nodes {
			mdl.Rules[i].BoolNodes = append(mdl.Rules[i].BoolNodes, n)
		}
	}

	// for each rule's paramtransitions and specialtransitions we need to find
	// the transition to point to - we do this based on the ParamTransition name and type
	for _, r := range mdl.Rules {
		for _, pt := range r.ParamTransitions {
			t := pt.Type
			tr := mdl.TransitionTry(t)
			if tr == nil {
				panic(errors.New("Model.Load() : TransitionTry failed!"))
			}
			pt.Transition = tr
		}
		for _, pt := range r.SpecialTransitions {
			t := pt.Type
			tr := mdl.TransitionSpTry(t)
			if tr == nil {
				panic(errors.New("Model.Load() : TransitionTry failed!"))
			}
			pt.Transition = tr
		}
	}

	//Now some additional processing of the special transitions
	//Because there isn't always a special transition for each parameter transition we need
	//to move the ones we found to the matching index and set the others to nil
	for _, r := range mdl.Rules {
		lp := len(r.ParamTransitions)
		ls := len(r.SpecialTransitions)
		// create nil transitions for those that don't exist
		if r.SpecialTransitions == nil {
			r.SpecialTransitions = make([]*ParamTransition, 0)
		}
		for i := ls; i < lp; i++ {
			pt := new(ParamTransition)
			r.SpecialTransitions = append(r.SpecialTransitions, pt)
		}
		// move the non-nil special transitions into place
		for i := ls - 1; i >= 0; i-- {
			nm := r.SpecialTransitions[i].Name
			idx := mdl.ParamIdx(nm)
			if idx == -1 {
				panic(errors.New("Model.Load() : ParamIdx lookup failed!"))
			}
			r.SpecialTransitions[idx].Name = r.SpecialTransitions[i].Name
			r.SpecialTransitions[idx].Type = r.SpecialTransitions[i].Type
			r.SpecialTransitions[idx].Transition = r.SpecialTransitions[i].Transition
			r.SpecialTransitions[i].Name = ""
			r.SpecialTransitions[i].Type = ""
			r.SpecialTransitions[i].Transition = nil
		}
	}

	for _, r := range mdl.Rules {
		for _, se := range r.ExprSymEquations {
			e := mdl.EquationTry(se.EqName)
			if e == nil {
				panic(errors.New("Model.Load() : EquationTry failed!"))
			}
			se.Equation = e
			se.Equation.SetFormula(se.Equation.Formula)
		}
	}

	return &mdl
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
func (mdl *Model) EvalEquationFormula(eq *Equation) float64 {

	return eq.EvalFormula(&mdl.FormulaVals)
}

// EquationGroupTry
func (mdl *Model) EquationGroupTry(nm string) *EqGrp {
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
				return tr
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
				return tr
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
			if r.EvalBoolExpr(postureSequence) {
				return r, i
			}
		}
	}
	return nil, 0
}
