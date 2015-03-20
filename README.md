# Aspis

Aspis is available from Clojars. Add the following dependency to your `project.clj`:

![Clojars Project](http://clojars.org/hostelrocket/aspis/latest-version.svg)

## Properties

There are a few non-standard properties supported by Aspis.

For example:
```clojure
(a-component :styles [{:color "red"} {:backgroundColor "green"}])
(a-component :style {:color "red" :backgroundColor "green"})
(a-component :css {:color "red" :backgroundColor "green"})
```
in all three cases, the style information can be retrieved as `this/props.style` in the render method of `a-component`.

The same is true of the class:
```clojure
(a-component :classes ["a" "b"])
(a-component :class "a b")
(a-component :className "a b")
```
in all three cases, the class can be retrieved from `this/props.className` in ther render method of `a-component`.

## Destructuring props

You can bind property names locally by providing a map destructuring form to `defelem`.  The `:or` key will be converted to React's `getDefaultProps` function.  The `:as` key will bind to the `this.props` Javascript object.

Furthermore, these bindings will be available in any other functions present in the `defelem` besides the render function.

```clojure
(aspis.core/defelem my-component 
                    {:keys [message] :or {message "Nothing to say"}}
    (h1 message))
```

## Extending props

Additional properties may be added onto an element via `aspis.core/with-props`.

```clojure
(aspis.core/with-props (a/div "Hello") {:color "red"})
```

## Atoms

To better support ClojureScript's idioms for state management, Aspis will automatically re-render any component when an atom its `:render` method depends on changes.  It does this by scanning the `:render` method for calls to `deref` (a.k.a `@`) and adding watches to those atoms on mount.

For instance,

```(def counter (atom 0))

(aspis.core/defelem my-component
    (button :onClick 
            (fn [event]
                (.preventDefault event)
                (swap! counter inc))
            (str "You have clicked " @counter " times")))
```

will re-render every time the `counter` atom changes value.  In this case, the atom is changed from within the component, but that is not essential.  External changes to `counter` will still cause `my-component` to re-render (via `.forceUpdate`).

Atoms in `props` or `state` which are `deref`'d in render will also trigger updates.
