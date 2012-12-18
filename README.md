# Emacs Lisp Weak References

This package provides two functions:

 * `weak-ref` -- Creates a weak reference to an object
 * `deref` -- Access the object behind a weak reference

See it in action:

```el
(setq ref (weak-ref (list 1 2 3)))
(deref ref) ; => (1 2 3)
(garbage-collect)
(deref ref) ; => nil
```

It's build on top of the weak references provided by hash tables.
