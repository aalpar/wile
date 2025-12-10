package values

var (
	_ Value = (*NativeError)(nil)
)

type NativeError struct {
	err     error
	message string
}

func NewNativeError(msg string) *NativeError {
	q := &NativeError{message: msg}
	return q
}

func (p *NativeError) Datum() error {
	return p.err
}

func (p *NativeError) Unwrap() error {
	return p.err
}

func (p *NativeError) Error() string {
	return p.message
}

func (p *NativeError) SchemeString() string {
	return "#<native-error>"
}

func (p *NativeError) IsVoid() bool {
	return p == nil
}

func (p *NativeError) EqualTo(o Value) bool {
	v, ok := o.(*NativeError)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	if p.err != v.err {
		return false
	}
	if p.message != v.message {
		return false
	}
	return true
}
