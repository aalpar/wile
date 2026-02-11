Yes, this is the gap we want to close. Today, Wile has coarse-grained capability control: extensions are opt-in. A bare `NewEngine()` can't touch the filesystem, read env vars, or call exit. You explicitly add what you need:

```go
  engine := wile.NewEngine(
      wile.WithExtension(files.Ext),
      wile.WithExtension(system.Ext),
  )
```

But once an extension is loaded, it's all-or-nothing. The next step is fine-grained authorization — a single Authorizer interface with a K8s-style verb+resource model that lets you do exactly what you described:

```go
  engine := wile.NewEngine(
      wile.WithExtension(files.Ext),
      wile.WithAuthorizer(security.Combine(
          security.NewFilesystemRoot("/data/config"),
          &security.ReadOnly{},
      )),
  )
```

Extensions define their own resource/action vocabulary, so when a network extension arrives it registers "net"/"connect" and the authorizer handles it — no interface changes. All the context plumbing is already in place.

Appreciate the feedback — please feel free to open an issue for specific security features you'd like to see and how you'd like them invoked.