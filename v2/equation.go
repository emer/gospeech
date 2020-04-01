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
	"errors"
	"fmt"
	"log"
	"strconv"
	"strings"
	"unicode"

	"github.com/goki/ki/kit"
)

const addChar rune = '+'
const subChar = '-'
const multChar = '*'
const divChar = '/'
const rtParenChar = ')'
const lftParenChar = '('

type Equation struct {
	Name string
	Comment string
	FormulaName string
	FormulaRoot *FormulaNode
}

type EqGroup struct {
	Name string
	Equations []Equation
}

type SymbolType int

const (
	SymInvalid = iota
	 
	SymAdd
	 
	SymSub
	 
	SymMult
	
	SymDiv
	
	SymRtParen
	
	SymLftParen
	
		SymString
	
	SymTypeN
)
//go:generate stringer -type=SymbolType

var Kit_SymbolType = kit.Enums.AddEnum(SymTypeN, kit.NotBitFlag, nil)

type FormulaNode interface {	// Parser returns the pi.Parser for this language
	Eval(syms *FormulaSymbolList) float64
}

type FormulaMinusUnaryOp struct {
	Child *FormulaNode
}

func NewFormulaMinusUnaryOp(c *FormulaNode) *FormulaMinusUnaryOp {
	f := FormulaMinusUnaryOp{}
	f.Child = c
	return &f
}

func (fmu *FormulaMinusUnaryOp) Eval(syms *FormulaSymbolList) float64 {
	return -(fmu.Eval(syms))
}

type FormulaAddOp struct {
	Child1 *FormulaNode
	Child2 *FormulaNode
}

func NewFormulaAddOp(c1, c2 *FormulaNode) *FormulaAddOp {
	f := FormulaAddOp{}
	f.Child1 = c1
	f.Child2 = c2
	return &f
}

func (fmu *FormulaAddOp) Eval(syms *FormulaSymbolList) float64 {
	return fmu.Eval(syms) + fmu.Eval(syms);
}

type FormulaSubOp struct {
	Child1 *FormulaNode
	Child2 *FormulaNode
}

func NewFormulaSubOp(c1, c2 *FormulaNode) *FormulaSubOp {
	f := FormulaSubOp{}
	f.Child1 = c1
	f.Child2 = c2
	return &f
}

func (fmu *FormulaSubOp) Eval(syms *FormulaSymbolList) float64 {
	return fmu.Eval(syms) - fmu.Eval(syms);
}

type FormulaMultOp struct {
	Child1 *FormulaNode
	Child2 *FormulaNode
}

func NewFormulaMultOp(c1, c2 *FormulaNode) *FormulaMultOp {
	f := FormulaMultOp{}
	f.Child1 = c1
	f.Child2 = c2
	return &f
}

func (fmu *FormulaMultOp) Eval(syms *FormulaSymbolList) float64 {
	return fmu.Eval(syms) * fmu.Eval(syms);
}

type FormulaDivOp struct {
	Child1 *FormulaNode
	Child2 *FormulaNode
}

func NewFormulaDivOp(c1, c2 *FormulaNode) *FormulaDivOp {
	f := FormulaDivOp{}
	f.Child1 = c1
	f.Child2 = c2
	return &f
}

func (fmu *FormulaDivOp) Eval(syms *FormulaSymbolList) float64 {
	return fmu.Eval(syms) / fmu.Eval(syms);
}

type FormulaConst struct {
	Value float64
}

func NewFormulaConst(value float64) *FormulaConst {
	f := FormulaConst{}
	f.Value = value
	return &f
}

func (fmu *FormulaConst) Eval(syms *FormulaSymbolList) float64 {
	return fmu.Value
}

type FormulaSymbolVal struct {
	Symbol FormulaSymbolType
}

func NewFormulaSymbolVal(symbol FormulaSymbolType) *FormulaSymbolVal {
	f := FormulaSymbolVal{}
	f.Symbol = symbol
	return &f
}

func (fmu *FormulaSymbolVal) Eval(syms *FormulaSymbolList) float64 {
	return syms.Symbols[int(fmu.Symbol)]
}

type FormulaParse struct {
	
}

type FormulaNodeParser struct {
	FormulaSymbolMap SymbolMap
	S string
	Pos int
	Symbol string
	SymbolType SymbolType
}

func NewFormulaNodeParser(s String) *FormulaNodeParser {
	fnp := FormulaNodeParser{}
	fnp.S = strings.TrimSpace(s)
	fnp.Pos = 0
	fnp.SymbolType = SymInvalid
	
	if len(fnp.S) == 0 {
		log.Println("Formula expression parser error: Empty string.")
		return
	}
	fnp.NextSymbol()
	return &fnp
}

func (fnp *FormulaNodeParser) ParseTerm() *FormulaNode {

	return nil
}

func (fnp *FormulaNodeParser) ParseExpr() *FormulaNode {

	return nil
}

func (fnp *FormulaNodeParser) Finished() bool {
	return fnp.Pos >= len(fnp.S)
}

// SkipSpaces moves the index into string past white space
func (fnp *FormulaNodeParser) SkipSpaces() {
	for !fnp.Finished() && unicode.IsSpace(rune(fnp.S[fnp.Pos])) {
		fnp.Pos++
	}
}
 
func (fnp *FormulaNodeParser) NextSymbol() {
	fnp.SkipSpaces()
	fnp.Symbol = ""

	if fnp.Finished() {
		fnp.SymbolType = SymInvalid
		return
	}

	c := rune(fnp.S[fnp.Pos])
	fnp.Pos++
	//fnp.SymType = c

	switch c {
	case addChar:
		fnp.SymbolType = SymAdd
	case subChar:
		fnp.SymbolType = SymSub
	case multChar:
		fnp.SymbolType = SymMult
	case divChar:
		fnp.SymbolType = SymDiv
	case rtParenChar:
		fnp.SymbolType = SymRtParen
	case lftParenChar:
		fnp.SymbolType = SymLftParen
	default:
		fnp.SymbolType = SymString
		cnext := string(fnp.S[fnp.Pos])  // notice that Pos has been incremented already
		for !fnp.Finished() && !IsSeparator(cnext)  {
			fnp.Symbol += cnext
			fnp.Pos++
		}
	}
}

// Parse Factor -- FACTOR -> "(" EXPRESSION ")" | SYMBOL | CONST | ADD_OP FACTOR
func (fnp *FormulaNodeParser) ParseFactor() (*FormulaNode, error) {
	switch fnp.SymbolType {
	case SymLftParen: // expression
		fnp.NextSymbol()
		 res := fnp.ParseExpression()
		if fnp.SymbolType != SymRtParen {
			return nil, errors.New("ParseFactor: Right parenthesis not found")
		}
		fnp.NextSymbol()
		return res, nil
	case SymAdd: // unary plus
		fnp.NextSymbol()
		return fnp.ParseFactor()
	case SymSub: // unary minus
		fnp.NextSymbol();
		return NewFormulaMinusUnaryOp(parseFactor())
	case SymString: // const / symbol
		temp := fnp.Symbol

// ToDo: !!!
		fnp.NextSymbol();
		FormulaSymbol::CodeMap::const_iterator iter = formulaSymbolMap_.find(symbolTmp);
		if (iter == formulaSymbolMap_.end()) {
			// It's not a symbol.
			return NewFormulaConst(strconv.ParseFloat(temp))
		} else {
			return NewFormulaSymbolValue(iter->second)
		}
	case SymRtParen:
		msg := fmt.Sprintf("ParseFactor: Unexpected symbol: %s", rtParenChar)
		return nil, errors.New(msg)
	case SymMult:
		msg := fmt.Sprintf("ParseFactor: Unexpected symbol: %s", multChar)
		return nil, errors.New(msg)
	case SymDiv:
		msg := fmt.Sprintf("ParseFactor: Unexpected symbol: %s", divChar)
		return nil, errors.New(msg)
	default:
		return nil, errors.New("Invalid Symbol")
	}
}

/*******************************************************************************
* TERM -> FACTOR { MULT_OP FACTOR }
*/
FormulaNode_ptr FormulaNodeParser::parseTerm()
{
	FormulaNode_ptr term1 = parseFactor();

	SymbolType type = symbolType_;
	while (type == SYMBOL_TYPE_MULT || type == SYMBOL_TYPE_DIV) {
		nextSymbol();
		FormulaNode_ptr term2 = parseFactor();
		FormulaNode_ptr expr;
		if (type == SYMBOL_TYPE_MULT) {
			expr.reset(new FormulaMultOp(std::move(term1), std::move(term2)));
		} else {
			expr.reset(new FormulaDivOp(std::move(term1), std::move(term2)));
		}
		term1.swap(expr);
		type = symbolType_;
	}

	return term1;
}

/*******************************************************************************
* EXPRESSION -> TERM { ADD_OP TERM }
*/
FormulaNode_ptr
FormulaNodeParser::parseExpression()
{
	FormulaNode_ptr term1 = parseTerm();

	SymbolType type = symbolType_;
	while (type == SYMBOL_TYPE_ADD || type == SYMBOL_TYPE_SUB) {

		nextSymbol();
		FormulaNode_ptr term2 = parseTerm();

		FormulaNode_ptr expr;
		if (type == SYMBOL_TYPE_ADD) {
			expr.reset(new FormulaAddOp(std::move(term1), std::move(term2)));
		} else {
			expr.reset(new FormulaSubOp(std::move(term1), std::move(term2)));
		}

		term1 = std::move(expr);
		type = symbolType_;
	}

	return term1;
}

FormulaNode_ptr
FormulaNodeParser::parse()
{
	FormulaNode_ptr formulaRoot = parseExpression();
	if (symbolType_ != SYMBOL_TYPE_INVALID) { // there is a symbol available
		throwException("Invalid text");
	}
	return formulaRoot;
}

} /* namespace */

func (eq *Equation) SetFormula(f string)
//void
//Equation::setFormula(const std::string& formula)
//{
	FormulaNodeParser p(formula);
	FormulaNode_ptr tempFormulaRoot = p.parse();

	formula_ = formula;
	std::swap(tempFormulaRoot, formulaRoot_);
}

//float
//Equation::evalFormula(const FormulaSymbolList& symbolList) const
//{
//	if (!formulaRoot_) {
//		THROW_EXCEPTION(InvalidStateException, "Empty formula.");
//	}
//
//	return formulaRoot_->eval(symbolList);
//}
//
//std::ostream&
//operator<<(std::ostream& out, const Equation& equation)
//{
//	if (equation.formulaRoot_) {
//		equation.formulaRoot_->print(out);
//	}
//	return out;
//}

