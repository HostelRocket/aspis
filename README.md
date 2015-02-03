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
