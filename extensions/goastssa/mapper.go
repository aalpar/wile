package goastssa

import (
	"fmt"
	"go/token"
	"go/types"

	"golang.org/x/tools/go/ssa"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/values"
)

type ssaMapper struct {
	fset      *token.FileSet
	positions bool
}

func (p *ssaMapper) mapFunction(fn *ssa.Function) values.Value {
	params := make([]values.Value, len(fn.Params))
	for i, param := range fn.Params {
		params[i] = goast.Node("ssa-param",
			goast.Field("name", goast.Str(param.Name())),
			goast.Field("type", goast.Str(types.TypeString(param.Type(), nil))),
		)
	}

	freeVars := make([]values.Value, len(fn.FreeVars))
	for i, fv := range fn.FreeVars {
		freeVars[i] = goast.Node("ssa-free-var",
			goast.Field("name", goast.Str(fv.Name())),
			goast.Field("type", goast.Str(types.TypeString(fv.Type(), nil))),
		)
	}

	blocks := make([]values.Value, len(fn.Blocks))
	for i, b := range fn.Blocks {
		blocks[i] = p.mapBlock(b)
	}

	fields := []values.Value{
		goast.Field("name", goast.Str(fn.Name())),
		goast.Field("signature", goast.Str(fn.Signature.String())),
		goast.Field("params", goast.ValueList(params)),
		goast.Field("free-vars", goast.ValueList(freeVars)),
		goast.Field("blocks", goast.ValueList(blocks)),
	}
	if fn.Pkg != nil {
		fields = append(fields, goast.Field("pkg", goast.Str(fn.Pkg.Pkg.Path())))
	}
	return goast.Node("ssa-func", fields...)
}

func (p *ssaMapper) mapBlock(b *ssa.BasicBlock) values.Value {
	preds := make([]values.Value, len(b.Preds))
	for i, pred := range b.Preds {
		preds[i] = values.NewInteger(int64(pred.Index))
	}
	succs := make([]values.Value, len(b.Succs))
	for i, succ := range b.Succs {
		succs[i] = values.NewInteger(int64(succ.Index))
	}
	instrs := make([]values.Value, 0, len(b.Instrs))
	for _, instr := range b.Instrs {
		if instr == nil {
			continue
		}
		instrs = append(instrs, p.mapInstruction(instr))
	}
	fields := []values.Value{
		goast.Field("index", values.NewInteger(int64(b.Index))),
		goast.Field("preds", goast.ValueList(preds)),
		goast.Field("succs", goast.ValueList(succs)),
		goast.Field("instrs", goast.ValueList(instrs)),
	}
	if b.Comment != "" {
		fields = append(fields, goast.Field("comment", goast.Str(b.Comment)))
	}
	return goast.Node("ssa-block", fields...)
}

// mapInstruction dispatches on SSA instruction type.
// Unmapped types produce (ssa-unknown ...) nodes.
func (p *ssaMapper) mapInstruction(instr ssa.Instruction) values.Value {
	switch v := instr.(type) {
	case *ssa.BinOp:
		return p.mapBinOp(v)
	case *ssa.UnOp:
		return p.mapUnOp(v)
	case *ssa.Alloc:
		return p.mapAlloc(v)
	case *ssa.Call:
		return p.mapCall(v)
	case *ssa.Store:
		return p.mapStore(v)
	case *ssa.FieldAddr:
		return p.mapFieldAddr(v)
	case *ssa.Field:
		return p.mapField(v)
	case *ssa.IndexAddr:
		return p.mapIndexAddr(v)
	case *ssa.Index:
		return p.mapIndex(v)
	case *ssa.Phi:
		return p.mapPhi(v)
	case *ssa.If:
		return p.mapIf(v)
	case *ssa.Jump:
		return p.mapJump(v)
	case *ssa.Return:
		return p.mapReturn(v)
	case *ssa.MakeMap:
		return p.mapMakeMap(v)
	case *ssa.MapUpdate:
		return p.mapMapUpdate(v)
	case *ssa.Lookup:
		return p.mapLookup(v)
	case *ssa.Extract:
		return p.mapExtract(v)
	case *ssa.MakeSlice:
		return p.mapMakeSlice(v)
	case *ssa.Slice:
		return p.mapSlice(v)
	case *ssa.MakeChan:
		return p.mapMakeChan(v)
	case *ssa.Send:
		return p.mapSend(v)
	case *ssa.Select:
		return p.mapSelect(v)
	case *ssa.Go:
		return p.mapGo(v)
	case *ssa.Defer:
		return p.mapDefer(v)
	case *ssa.RunDefers:
		return p.mapRunDefers(v)
	case *ssa.Range:
		return p.mapRange(v)
	case *ssa.Next:
		return p.mapNext(v)
	case *ssa.Panic:
		return p.mapPanic(v)
	case *ssa.ChangeType:
		return p.mapChangeType(v)
	case *ssa.Convert:
		return p.mapConvert(v)
	case *ssa.ChangeInterface:
		return p.mapChangeInterface(v)
	case *ssa.SliceToArrayPointer:
		return p.mapSliceToArrayPointer(v)
	case *ssa.MakeInterface:
		return p.mapMakeInterface(v)
	case *ssa.TypeAssert:
		return p.mapTypeAssert(v)
	case *ssa.MakeClosure:
		return p.mapMakeClosure(v)
	case *ssa.MultiConvert:
		return p.mapMultiConvert(v)
	case *ssa.DebugRef:
		return p.mapDebugRef(v)
	default:
		return p.mapUnknown(instr)
	}
}

func (p *ssaMapper) mapCall(v *ssa.Call) values.Value {
	fields := p.mapCallCommon(&v.Call)
	fields = append(fields,
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
	)
	return goast.Node("ssa-call", fields...)
}

func (p *ssaMapper) mapCallCommon(c *ssa.CallCommon) []values.Value {
	args := make([]values.Value, len(c.Args))
	operands := make([]values.Value, 0, len(c.Args)+1)

	for i, a := range c.Args {
		args[i] = valName(a)
		operands = append(operands, valName(a))
	}

	fields := []values.Value{
		goast.Field("args", goast.ValueList(args)),
	}

	if c.IsInvoke() {
		// Interface method call.
		fields = append(fields,
			goast.Field("mode", goast.Sym("invoke")),
			goast.Field("method", goast.Str(c.Method.Name())),
			goast.Field("recv", valName(c.Value)),
		)
		operands = append(operands, valName(c.Value))
	} else {
		// Static or dynamic function call.
		fields = append(fields,
			goast.Field("mode", goast.Sym("call")),
			goast.Field("func", valName(c.Value)),
		)
		operands = append(operands, valName(c.Value))
	}
	fields = append(fields, goast.Field("operands", goast.ValueList(operands)))
	return fields
}

func (p *ssaMapper) mapBinOp(v *ssa.BinOp) values.Value {
	return goast.Node("ssa-binop",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("op", goast.Sym(v.Op.String())),
		goast.Field("x", valName(v.X)),
		goast.Field("y", valName(v.Y)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X), valName(v.Y)})),
	)
}

func (p *ssaMapper) mapUnOp(v *ssa.UnOp) values.Value {
	return goast.Node("ssa-unop",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("op", goast.Sym(v.Op.String())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapAlloc(v *ssa.Alloc) values.Value {
	return goast.Node("ssa-alloc",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("heap", values.BoolToBoolean(v.Heap)),
		goast.Field("operands", values.EmptyList),
	)
}

func (p *ssaMapper) mapStore(v *ssa.Store) values.Value {
	return goast.Node("ssa-store",
		goast.Field("addr", valName(v.Addr)),
		goast.Field("val", valName(v.Val)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Addr), valName(v.Val)})),
	)
}

func (p *ssaMapper) mapFieldAddr(v *ssa.FieldAddr) values.Value {
	structType := typesDeref(v.X.Type())
	fieldName := fieldNameAt(structType, v.Field)
	return goast.Node("ssa-field-addr",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("field", goast.Str(fieldName)),
		goast.Field("field-index", values.NewInteger(int64(v.Field))),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapField(v *ssa.Field) values.Value {
	structType := v.X.Type()
	fieldName := fieldNameAt(structType, v.Field)
	return goast.Node("ssa-field",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("field", goast.Str(fieldName)),
		goast.Field("field-index", values.NewInteger(int64(v.Field))),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapIndexAddr(v *ssa.IndexAddr) values.Value {
	return goast.Node("ssa-index-addr",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("index", valName(v.Index)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X), valName(v.Index)})),
	)
}

func (p *ssaMapper) mapIndex(v *ssa.Index) values.Value {
	return goast.Node("ssa-index",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("index", valName(v.Index)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X), valName(v.Index)})),
	)
}

// typesDeref dereferences a pointer type to get the element type.
func typesDeref(t types.Type) types.Type {
	pt, ok := t.Underlying().(*types.Pointer)
	if ok {
		return pt.Elem()
	}
	return t
}

// fieldNameAt returns the field name at index i in a struct type.
func fieldNameAt(t types.Type, i int) string {
	st, ok := t.Underlying().(*types.Struct)
	if !ok {
		return fmt.Sprintf("field_%d", i)
	}
	if i < st.NumFields() {
		return st.Field(i).Name()
	}
	return fmt.Sprintf("field_%d", i)
}

func (p *ssaMapper) mapPhi(v *ssa.Phi) values.Value {
	edges := make([]values.Value, len(v.Edges))
	operands := make([]values.Value, len(v.Edges))
	for i, e := range v.Edges {
		blockIdx := values.NewInteger(int64(v.Block().Preds[i].Index))
		edges[i] = values.NewCons(blockIdx, valName(e))
		operands[i] = valName(e)
	}
	fields := []values.Value{
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("edges", goast.ValueList(edges)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList(operands)),
	}
	if v.Comment != "" {
		fields = append(fields, goast.Field("comment", goast.Str(v.Comment)))
	}
	return goast.Node("ssa-phi", fields...)
}

func (p *ssaMapper) mapIf(v *ssa.If) values.Value {
	return goast.Node("ssa-if",
		goast.Field("cond", valName(v.Cond)),
		goast.Field("then", values.NewInteger(int64(v.Block().Succs[0].Index))),
		goast.Field("else", values.NewInteger(int64(v.Block().Succs[1].Index))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Cond)})),
	)
}

func (p *ssaMapper) mapJump(v *ssa.Jump) values.Value {
	return goast.Node("ssa-jump",
		goast.Field("target", values.NewInteger(int64(v.Block().Succs[0].Index))),
		goast.Field("operands", values.EmptyList),
	)
}

func (p *ssaMapper) mapReturn(v *ssa.Return) values.Value {
	results := make([]values.Value, len(v.Results))
	operands := make([]values.Value, len(v.Results))
	for i, r := range v.Results {
		results[i] = valName(r)
		operands[i] = valName(r)
	}
	return goast.Node("ssa-return",
		goast.Field("results", goast.ValueList(results)),
		goast.Field("operands", goast.ValueList(operands)),
	)
}

func (p *ssaMapper) mapMakeMap(v *ssa.MakeMap) values.Value {
	return goast.Node("ssa-make-map",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("reserve", valName(v.Reserve)),
		goast.Field("operands", values.EmptyList),
	)
}

func (p *ssaMapper) mapMapUpdate(v *ssa.MapUpdate) values.Value {
	return goast.Node("ssa-map-update",
		goast.Field("map", valName(v.Map)),
		goast.Field("key", valName(v.Key)),
		goast.Field("val", valName(v.Value)),
		goast.Field("operands", goast.ValueList([]values.Value{
			valName(v.Map), valName(v.Key), valName(v.Value),
		})),
	)
}

func (p *ssaMapper) mapLookup(v *ssa.Lookup) values.Value {
	return goast.Node("ssa-lookup",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("index", valName(v.Index)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("comma-ok", values.BoolToBoolean(v.CommaOk)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X), valName(v.Index)})),
	)
}

func (p *ssaMapper) mapExtract(v *ssa.Extract) values.Value {
	return goast.Node("ssa-extract",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("tup", valName(v.Tuple)),
		goast.Field("index", values.NewInteger(int64(v.Index))),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Tuple)})),
	)
}

func (p *ssaMapper) mapMakeSlice(v *ssa.MakeSlice) values.Value {
	return goast.Node("ssa-make-slice",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("len", valName(v.Len)),
		goast.Field("cap", valName(v.Cap)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Len), valName(v.Cap)})),
	)
}

func (p *ssaMapper) mapSlice(v *ssa.Slice) values.Value {
	return goast.Node("ssa-slice",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("low", valName(v.Low)),
		goast.Field("high", valName(v.High)),
		goast.Field("max", valName(v.Max)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapMakeChan(v *ssa.MakeChan) values.Value {
	return goast.Node("ssa-make-chan",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("size", valName(v.Size)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Size)})),
	)
}

func (p *ssaMapper) mapSend(v *ssa.Send) values.Value {
	return goast.Node("ssa-send",
		goast.Field("chan", valName(v.Chan)),
		goast.Field("x", valName(v.X)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Chan), valName(v.X)})),
	)
}

func (p *ssaMapper) mapSelect(v *ssa.Select) values.Value {
	states := make([]values.Value, len(v.States))
	var operands []values.Value
	for i, s := range v.States {
		stateFields := []values.Value{
			goast.Field("chan", valName(s.Chan)),
			goast.Field("dir", goast.Sym(chanDirStr(s.Dir))),
		}
		operands = append(operands, valName(s.Chan))
		if s.Send != nil {
			stateFields = append(stateFields, goast.Field("send", valName(s.Send)))
			operands = append(operands, valName(s.Send))
		}
		states[i] = goast.Node("ssa-select-state", stateFields...)
	}
	return goast.Node("ssa-select",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("blocking", values.BoolToBoolean(v.Blocking)),
		goast.Field("states", goast.ValueList(states)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList(operands)),
	)
}

func (p *ssaMapper) mapGo(v *ssa.Go) values.Value {
	fields := p.mapCallCommon(&v.Call)
	return goast.Node("ssa-go", fields...)
}

func (p *ssaMapper) mapDefer(v *ssa.Defer) values.Value {
	fields := p.mapCallCommon(&v.Call)
	return goast.Node("ssa-defer", fields...)
}

func (p *ssaMapper) mapRunDefers(_ *ssa.RunDefers) values.Value {
	return goast.Node("ssa-run-defers",
		goast.Field("operands", values.EmptyList),
	)
}

func (p *ssaMapper) mapRange(v *ssa.Range) values.Value {
	return goast.Node("ssa-range",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapNext(v *ssa.Next) values.Value {
	return goast.Node("ssa-next",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("iter", valName(v.Iter)),
		goast.Field("is-string", values.BoolToBoolean(v.IsString)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Iter)})),
	)
}

func (p *ssaMapper) mapPanic(v *ssa.Panic) values.Value {
	return goast.Node("ssa-panic",
		goast.Field("x", valName(v.X)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapChangeType(v *ssa.ChangeType) values.Value {
	return goast.Node("ssa-change-type",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapConvert(v *ssa.Convert) values.Value {
	return goast.Node("ssa-convert",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapChangeInterface(v *ssa.ChangeInterface) values.Value {
	return goast.Node("ssa-change-interface",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapSliceToArrayPointer(v *ssa.SliceToArrayPointer) values.Value {
	return goast.Node("ssa-slice-to-array-ptr",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapMakeInterface(v *ssa.MakeInterface) values.Value {
	return goast.Node("ssa-make-interface",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapTypeAssert(v *ssa.TypeAssert) values.Value {
	return goast.Node("ssa-type-assert",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("asserted-type", goast.Str(types.TypeString(v.AssertedType, nil))),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("comma-ok", values.BoolToBoolean(v.CommaOk)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapMakeClosure(v *ssa.MakeClosure) values.Value {
	bindings := make([]values.Value, len(v.Bindings))
	operands := make([]values.Value, len(v.Bindings))
	for i, b := range v.Bindings {
		bindings[i] = valName(b)
		operands[i] = valName(b)
	}
	return goast.Node("ssa-make-closure",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("fn", valName(v.Fn)),
		goast.Field("bindings", goast.ValueList(bindings)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList(operands)),
	)
}

// mapMultiConvert handles generic multi-type conversions (same shape as Convert).
func (p *ssaMapper) mapMultiConvert(v *ssa.MultiConvert) values.Value {
	return goast.Node("ssa-multi-convert",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

// mapDebugRef handles source-level debug annotations.
// Only appears when SSA is built with ssa.GlobalDebug; included for completeness.
func (p *ssaMapper) mapDebugRef(v *ssa.DebugRef) values.Value {
	return goast.Node("ssa-debug-ref",
		goast.Field("x", valName(v.X)),
		goast.Field("is-addr", values.BoolToBoolean(v.IsAddr)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

// chanDirStr converts a channel direction to a Scheme symbol string.
func chanDirStr(dir types.ChanDir) string {
	switch dir {
	case types.SendOnly:
		return "send"
	case types.RecvOnly:
		return "recv"
	default:
		return "both"
	}
}

func (p *ssaMapper) mapUnknown(instr ssa.Instruction) values.Value {
	fields := []values.Value{
		goast.Field("go-type", goast.Str(fmt.Sprintf("%T", instr))),
	}
	v, ok := instr.(ssa.Value)
	if ok {
		fields = append(fields,
			goast.Field("name", goast.Str(v.Name())),
			goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		)
	}
	fields = append(fields, goast.Field("operands", values.EmptyList))
	return goast.Node("ssa-unknown", fields...)
}

// valName returns the SSA value name for use as an operand reference.
func valName(v ssa.Value) values.Value {
	if v == nil {
		return values.FalseValue
	}
	return goast.Str(v.Name())
}
