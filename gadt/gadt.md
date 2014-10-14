 Basic GADT Challenge
======================


 - I have a _bag_ of stuff.
 - My bag is _awesome_ it can hold different types of things allocated by a
   name.
 - My bag is kind of ok, in that it can only hold certain types of things.
   Specifically it can hold Int's, String's and Bool's. But this also makes
   it convenient to access those things.
 - Conveniently the key tells me what type to expect.

In my haskell2010 world, I implement this as a simple data type:

```
data Key = Key String
data Value = I Int | S String | B Bool
data Bag = Bag [(Key, Value)]
```

And I can implement things like:

```
data Result a =
    NotFound
  | InvalidType String
  | Value a

getBool :: Bag -> Key -> a Bool
```

But this is obviously less than desirable, I have two errors to handle:
 1. Whether the value exists or not
 2. Whether the value was a bool

I also didn't use the info, that I know the type based on my key.

Lets build better a Key data type such that it encodes the type of the value,
and then change value, such that it can _only_ be indexed by an appropriately
typed key.


The point of this exercise is to:

 - Learn how to encode an invariant using GADTs and phantom types.

 - Hide that invariant using existential types.

 - Use pattern matching to "re-learn" about your extra invariant in your
   implementation such that we can build a better API (for example an API
   where we can't get store the wrong type data against a key.

 - Experiment with why you would use GADTs over more traditional data types.

 - Discuss the balance of trading off forcing error handling as "proof"
   carying code vs returning error conditions.
