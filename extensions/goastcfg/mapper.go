package goastcfg

import (
	"go/token"

	"golang.org/x/tools/go/ssa"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/values"
)

type cfgMapper struct {
	fset      *token.FileSet
	positions bool
}

// mapFunction maps all basic blocks of an SSA function to cfg-block s-expressions.
// fn.Recover, if present, is appended last and tagged with (recover . #t).
func (p *cfgMapper) mapFunction(fn *ssa.Function) values.Value {
	blocks := make([]values.Value, len(fn.Blocks))
	for i, b := range fn.Blocks {
		blocks[i] = p.mapBlock(b, false)
	}
	if fn.Recover != nil {
		blocks = append(blocks, p.mapBlock(fn.Recover, true))
	}
	return goast.ValueList(blocks)
}

// mapBlock maps a single SSA basic block to a cfg-block s-expression.
// isRecover must be true when b is fn.Recover; it adds (recover . #t) to the
// field list to distinguish it from the entry block (both have idom=#f).
func (p *cfgMapper) mapBlock(b *ssa.BasicBlock, isRecover bool) values.Value {
	preds := make([]values.Value, len(b.Preds))
	for i, pred := range b.Preds {
		preds[i] = values.NewInteger(int64(pred.Index))
	}
	succs := make([]values.Value, len(b.Succs))
	for i, succ := range b.Succs {
		succs[i] = values.NewInteger(int64(succ.Index))
	}

	var idom values.Value
	if idomBlock := b.Idom(); idomBlock != nil {
		idom = values.NewInteger(int64(idomBlock.Index))
	} else {
		idom = values.FalseValue
	}

	fields := []values.Value{
		goast.Field("index", values.NewInteger(int64(b.Index))),
		goast.Field("preds", goast.ValueList(preds)),
		goast.Field("succs", goast.ValueList(succs)),
		goast.Field("idom", idom),
	}
	if isRecover {
		fields = append(fields, goast.Field("recover", values.TrueValue))
	}
	if b.Comment != "" {
		fields = append(fields, goast.Field("comment", goast.Str(b.Comment)))
	}
	if p.positions && b.Instrs != nil && len(b.Instrs) > 0 {
		pos := b.Instrs[0].Pos()
		if pos.IsValid() {
			fields = append(fields, goast.Field("pos", goast.Str(p.fset.Position(pos).String())))
		}
	}
	return goast.Node("cfg-block", fields...)
}
