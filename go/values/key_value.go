package values

type KeyValue struct {
	Key   string
	Value Value
}

func NewKeyValue(key string, value Value) *KeyValue {
	return &KeyValue{
		Key:   key,
		Value: value,
	}
}

func (p *KeyValue) Datum() map[string]Value {
	return map[string]Value{p.Key: p.Value}
}
