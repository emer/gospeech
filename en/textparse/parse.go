package textparse

// ToDo: All of this is a stub to get up and running
type TextParser struct {
	table map[string]string
}

func NewTextParser() *TextParser {
	tp := TextParser{}
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
