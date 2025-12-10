package environment

type MetaFrame struct {
	parent *MetaFrame
}

func NewMetaFrame() *MetaFrame {
	q := &MetaFrame{}
	return q
}
