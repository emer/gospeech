package v2

import (
	"encoding/xml"
	"log"
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
	Name        string `xml:"name,attr"`
	Comment     string `xml:"comment"`
	Formula     string `xml:"formula,attr"`
	FormulaRoot *FormulaNode
}

type EqGrp struct {
	XMLName   xml.Name   `xml:"equation-group"`
	Name      string     `xml:"name,attr"`
	Equations []Equation `xml:"equation,attr"`
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

type FormulaNode struct { // Parser returns the pi.Parser for this language
}

func (fn *FormulaNode) Eval(sl *FormulaSymbols) float64 {
	return 0.0
}

type FormulaMinusUnaryOp struct {
	FormulaNode
	Child *FormulaNode
}

func NewFormulaMinusUnaryOp(c *FormulaNode) *FormulaMinusUnaryOp {
	f := FormulaMinusUnaryOp{}
	f.Child = c
	return &f
}

func (fmu *FormulaMinusUnaryOp) Eval(sl *FormulaSymbols) float64 {
	return -(fmu.Eval(sl))
}

type FormulaAddOp struct {
	FormulaNode
	Child1 *FormulaNode
	Child2 *FormulaNode
}

func NewFormulaAddOp(c1, c2 *FormulaNode) *FormulaAddOp {
	f := FormulaAddOp{}
	f.Child1 = c1
	f.Child2 = c2
	return &f
}

func (fmu *FormulaAddOp) Eval(sl *FormulaSymbols) float64 {
	return fmu.Eval(sl) + fmu.Eval(sl)
}

type FormulaSubOp struct {
	FormulaNode
	Child1 *FormulaNode
	Child2 *FormulaNode
}

func NewFormulaSubOp(c1, c2 *FormulaNode) *FormulaSubOp {
	f := FormulaSubOp{}
	f.Child1 = c1
	f.Child2 = c2
	return &f
}

func (fmu *FormulaSubOp) Eval(sl *FormulaSymbols) float64 {
	return fmu.Eval(sl) - fmu.Eval(sl)
}

type FormulaMultOp struct {
	FormulaNode
	Child1 *FormulaNode
	Child2 *FormulaNode
}

func NewFormulaMultOp(c1, c2 *FormulaNode) *FormulaMultOp {
	f := FormulaMultOp{}
	f.Child1 = c1
	f.Child2 = c2
	return &f
}

func (fmu *FormulaMultOp) Eval(sl *FormulaSymbols) float64 {
	return fmu.Eval(sl) * fmu.Eval(sl)
}

type FormulaDivOp struct {
	FormulaNode
	Child1 *FormulaNode
	Child2 *FormulaNode
}

func NewFormulaDivOp(c1, c2 *FormulaNode) *FormulaDivOp {
	f := FormulaDivOp{}
	f.Child1 = c1
	f.Child2 = c2
	return &f
}

func (fmu *FormulaDivOp) Eval(sl *FormulaSymbols) float64 {
	return fmu.Eval(sl) / fmu.Eval(sl)
}

type FormulaConst struct {
	FormulaNode
	Value float64
}

func NewFormulaConst(value float64) *FormulaConst {
	f := FormulaConst{}
	f.Value = value
	return &f
}

func (fmu *FormulaConst) Eval(sl *FormulaSymbols) float64 {
	return fmu.Value
}

type FormulaSymbolVal struct {
	FormulaNode
	Symbol FormulaSymbolType
}

func NewFormulaSymbolVal(symbol FormulaSymbolType) *FormulaSymbolVal {
	f := FormulaSymbolVal{}
	f.Symbol = symbol
	return &f
}

func (fmu *FormulaSymbolVal) Eval(sl *FormulaSymbols) float64 {
	return sl.CodeMap[fmu.Symbol]
}

type FormulaNodeParser struct {
	FormulaSymbols FormulaSymbols
	S              string
	Pos            int
	Symbol         string
	SymbolType     SymbolType
}

func NewFormulaNodeParser(s string) *FormulaNodeParser {
	fnp := FormulaNodeParser{}
	fnp.S = strings.TrimSpace(s)
	fnp.Pos = 0
	fnp.SymbolType = SymInvalid

	if len(fnp.S) == 0 {
		log.Println("Formula expression parser error: Empty string.")
		return nil
	}
	fnp.NextSymbol()
	return &fnp
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
		cnext := string(fnp.S[fnp.Pos]) // notice that Pos has been incremented already
		for !fnp.Finished() && !IsSeparator(cnext) {
			fnp.Symbol += cnext
			fnp.Pos++
		}
	}
}

// Parse Factor -- FACTOR -> "(" EXPRESSION ")" | SYMBOL | CONST | ADD_OP FACTOR
func (fnp *FormulaNodeParser) ParseFactor() *FormulaNode {
	switch fnp.SymbolType {
	case SymLftParen: // expression
		fnp.NextSymbol()
		res := fnp.ParseExpr()
		if fnp.SymbolType != SymRtParen {
			//return nil, errors.New("ParseFactor: Right parenthesis not found")
			return nil
		}
		fnp.NextSymbol()
		return res
	case SymAdd: // unary plus
		fnp.NextSymbol()
		return fnp.ParseFactor()
	case SymSub: // unary minus
		fnp.NextSymbol()
		op := NewFormulaMinusUnaryOp(fnp.ParseFactor())
		return &op.FormulaNode
	case SymString: // const / symbol
		//temp := fnp.Symbol

		// ToDo: !!!
		fnp.NextSymbol()

	//	for k, v := range fnp.FormulaSymbols.Syms { // .Syms is the map
	//		if k == fnp.Symbol {
	//			return NewFormulaConst(strconv.ParseFloat(temp))
	//}
	//	if (iter == formulaSymbolMap_.end()) {
	//		// It's not a symbol.
	//		return NewFormulaConst(strconv.ParseFloat(temp))
	//	} else {
	//		return NewFormulaSymbolValue(iter->second)
	//	}
	case SymRtParen:
		//msg := fmt.Sprintf("ParseFactor: Unexpected symbol: %s", rtParenChar)
		//return nil, errors.New(msg)
		return nil
	case SymMult:
		//msg := fmt.Sprintf("ParseFactor: Unexpected symbol: %s", multChar)
		return nil
	case SymDiv:
		//msg := fmt.Sprintf("ParseFactor: Unexpected symbol: %s", divChar)
		//return nil, errors.New(msg)
		return nil
	default:
		//return nil, errors.New("Invalid Symbol")
		return nil
	}
	return nil
}

// ParseTerm TERM -> FACTOR { MULT_OP FACTOR }
func (fnp *FormulaNodeParser) ParseTerm() *FormulaNode {
	term1 := fnp.ParseFactor()

	symType := fnp.SymbolType
	for symType == SymMult || symType == SymDiv {
		fnp.NextSymbol()
		term2 := fnp.ParseFactor()
		var expr *FormulaNode
		if symType == SymMult {
			op := NewFormulaMultOp(term1, term2)
			expr = &op.FormulaNode
		} else {
			op := NewFormulaDivOp(term1, term2)
			expr = &op.FormulaNode

		}
		temp := term1
		term1 = expr
		expr = temp
		symType = fnp.SymbolType
	}

	return term1
}

// ParseExpr  EXPRESSION -> TERM { ADD_OP TERM }
func (fnp *FormulaNodeParser) ParseExpr() *FormulaNode {
	term1 := fnp.ParseTerm()

	symType := fnp.SymbolType
	for symType == SymAdd || symType == SymSub {
		fnp.NextSymbol()
		term2 := fnp.ParseTerm()

		var expr *FormulaNode
		if symType == SymMult {
			//expr = NewFormulaAddOp(term1, term2).(*FormulaNode.FormulaAddOp)
			op := NewFormulaAddOp(term1, term2)
			expr = &op.FormulaNode
		} else {
			//expr = NewFormulaSubOp(term1, term2).(*FormulaNode.FormulaSubOp)
			op := NewFormulaSubOp(term1, term2)
			expr = &op.FormulaNode
		}
		temp := term1
		term1 = expr
		expr = temp
		symType = fnp.SymbolType
	}

	return term1
}

// Parse
func (fnp *FormulaNodeParser) Parse() *FormulaNode {
	formulaRoot := fnp.ParseExpr()
	if fnp.SymbolType != SymInvalid { // hmmm, seems backwards
		log.Println("Parse: Invalid text")
		return nil
	}
	return formulaRoot
}

func (eq *Equation) SetFormula(formula string) {
	np := NewFormulaNodeParser(formula)
	tempFormulaRoot := np.Parse()
	eq.Formula = formula

	temp := eq.FormulaRoot
	eq.FormulaRoot = tempFormulaRoot
	tempFormulaRoot = temp
}

func (eq *Equation) Eval(sl *FormulaSymbols) float64 {
	if eq.FormulaRoot == nil {
		panic("EmptyFormula")
	}

	// ToDo: what to do here
	// NextSymbol and switch on symbol?
	//return eq.FormulaRoot.Eval(sl)
	return 0.0 // temp to compile
}
