package values

import (
	"fmt"
)

var (
	_ Value = (*Error)(nil)
)

// Error represents an error value in the values package.
type Error struct {
	Root    error
	Cause   *Error
	Message *String
}

func NewError(msg string) *Error {
	q := &Error{
		Message: NewString(msg),
	}
	return q
}

func NewErrorf(msg string, vs ...any) *Error {
	if len(vs) == 0 {
		return NewError(msg)
	}
	q := &Error{
		Message: NewString(fmt.Sprintf(msg, vs...)),
	}
	return q
}

func WrapErrorf(err error, msg string, vs ...any) *Error {
	if err == nil {
		return NewErrorf(msg, vs...)
	}
	return &Error{
		Root:    err,
		Message: NewString(fmt.Sprintf(msg, vs...)),
	}
}

func (p *Error) Datum() string {
	return p.Message.Datum()
}

func (p *Error) Unwrap() *String {
	return p.Message
}

func (q *Error) Error() string {
	return q.Message.Value
}

func (p *Error) IsVoid() bool {
	return p == nil
}

func (p *Error) EqualTo(v Value) bool {
	other, ok := v.(*Error)
	if !ok {
		return false
	}
	return p.Message == other.Message
}

func (p *Error) SchemeString() string {
	return p.Message.SchemeString()
}
