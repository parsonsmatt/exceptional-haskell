# Unchecked
# IO
# Exceptions

Note:

Well, typed and checked exceptions have some fairly significant problems in Haskell.
Let's explore the unchecked exceptions system and see how it works out in practice.


```haskell
openFile :: FilePath -> IOMode -> IO Handle

main = do
    handle <- openFile "wat.txt" ReadMode
    contents <- hGetContents handle
    putStrLn contents
```

Note:

This is a convenient bit of code to write!
Now, this type signature is a little deceiving.
What if the file doesn't exist?
What if we don't have permission to access it?
There's all kinds of reasons why we shouldn't be able to return a `Handle` for any given `FilePath`.
But this code doesn't seem to reflect that, at all.

Technically, the IO type's contract includes any runtime exception at all -- but this is somewhat unsatisfying, as that applies to literally any value in Haskell.
We still want to minimize the spooky action that's not tracked by the compiler.


```haskell
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception.Safe 
    (try, IOError)

main = do
    ehandle <- try (openFile "wat.txt" ReadMode)
    case ehandle of
        Left (err :: IOError) ->
            putStrLn "File isn't real!"
        Right handle -> do
            contents <- hGetContents handle
            putStrLn contents
```

Note:

The package "safe-exceptions" has reasonable functions for working with Haskell's runtime IO exceptions.
Here, we're using `try`, and we tell it to only catch IOError.


```haskell
main = do
    doSomeAction `catch` \(e :: ArithException) ->
        print e

    mightFail `finally` do
        putStrLn "This runs even if an exception "
        putStrLn "is thrown"

    bracket 
        acquireResource
        (\resource -> release resource)
        (\resource -> use resource)
```

Note:

We interact with runtime exceptions in the same way as many programming languages.
We can say try, `catch`, `finally`, and we have a `try-with-resource` thing called `bracket`

`catch` will run the first argument, and take a handler for an exception of a specific type.
If an exception is thrown, it uses the result from the exception handler instead.

`finally` runs the second block whenever the first block finishes, whether that was from successful completion or from an exception.

With try and catch, the Haskell runtime uses the *type* of the exception to figure out what it needs to catch.
We must specify the type at compile-time for this to work.
How does it know that, just from the type???


```haskell
data AnyException where
    AnyException :: Typeable e => e -> AnyException

throw :: Typeable e => e -> Either AnyException a
throw e = Left (AnyException e)

try :: forall error a. Typeable error 
    => Either AnyException               a 
    -> Either AnyException (Either error a)
try action =
    case action of
        Left (AnyException (err :: thrown)) ->
            case eqT @error @thrown of
                Nothing   -> 
                    Left (AnyException err)
                Just Refl -> 
                    Right (Left err)
        Right a ->
            Right (Right a)
```

Note:

Haskell's exceptions use the `Typeable` machinery to do runtime type analysis.
We define our own IO type, which is actually just Either where the left type is a custom exception.
The AnyException type is a GADT -- these can do neat tricks, like hiding type variables and packing dictionaries in there.
When we pattern match on a GADT, we can "open up" those packed up dictionaries and types.

When we throw an exception, we package it inside the `AnyException` constructor.
This also stuffs the Typeable dictionary in.

When we "try" an action, we are saying: I think I know what the exception is that's hidden in the `Left`, if one is.
If the type lines up, then we get back a `Right (Left err)` of the underlying error.
We use the `eqT` function from the Typeable module, and we use the TypeApplications syntax to apply the two type variables to see if it's the same type.

This is the basic gist of how GHC's IO exceptions are implemented, in terms of the user-facing API, anyway.
You guess (or have found through experience, or digging around in the source) what exceptions something throws, and you catch and handle them.


## Dynamically Typed

## Subtypes

## Not tracked in the types

## No stack traces
<!-- .element: class="fragment" -->

Note:

Haskell's exception system feels like a prank.
We have a purely functional, statically typed programming language that eschews subtyping in favor of type inference.
And then we gave it an exception system that's dynamically typed, uses subtyping, and isn't tracked in the types at all!
It's exactly like what most OOP languages have for an exception system.

Well, almost exactly.
No stack traces.


# Best Practices

# aka
<!-- .element: class="fragment" -->

# Some Guy's Opinions
<!-- .element: class="fragment" -->

Note:

I'm going to share some exception best practices with you now for designing your runtime exception types.
And by "best practices" I mean "just my opinions"
These are mostly informed by pain points that I've felt when debugging exceptions or catching them.


# Don't

Note:

Don't.
Don't throw an exception.
Do you really need to?
Can you use Either, or Maybe?
Do that instead.
Can you impose some constraints on your inputs that prevent the error case?
Do that instead.
Just because we can, doesn't mean we should!


# Don't

## use `error`

## or strings

Note:

If you're going to throw an exception, don't use the error function.
Don't use strings, or Text, or whatever.
The only thing you can do with a string is print it to the console to show to the developer.
You can do this with anything that has a Show instance.
So throw meaningful errors.


# Leave a Comment

```haskell
-- | This function opens a file. If the file 
-- isn't present, it throws 'FileNotFound' error.
openFile :: FilePath -> IOMode -> IO Handle
```

Note:

Please write a comment saying what exception you throw, preferably using the Haddock link syntax so I can easily find the constructors and type to catch.


# Consider Purity

## Bad

```haskell
module Data.Map where

data KeyNotFoundError = KeyNotFoundError

-- | Looks up an item in the 'Map'. If the
-- key isn't present, throws 'KeyNotFoundError'.
lookup :: Ord k => k -> Map k v -> v
```

Note:

We really would prefer to restrict exceptions to exceptional situations.
This definition of lookup for a map throws an exception if the value isn't found, similar to Python.
In Haskell, we'd much rather use `Maybe` to express this.


# Consider Purity

## Good

```haskell
module Data.Map where

-- | Looks up an item in the 'Map'. If the
-- key isn't present, returns 'Nothing'.
lookup :: Ord k => k -> Map k v -> Maybe v
```

Note:

Here, we've used `Maybe` to indicate failure instead of throwing an exception.
This is much better.
We should expect that users will look up keys somewhat regularly; it's not an "exceptional" situation.
Let's use that insight to design a better `openFile` function.


# Purify

```haskell
data FileSystem 
    = File      Name Permissions Handle
    | Directory Name Permissions [FileSystem]

type FilePath = [Name]

openFile 
    :: FilePath 
    -> IOMode
    -> UserRole 
    -> FileSystem 
    -> Handle
```

Note:

First, we "purify" the file system.
We give it a model in Haskell types, and then we take it as an explicit parameter.
In this model, a filesystem is either a file with a name, permission info, and contents.
Or it is a directory with a name, permissions, and a list of file systems.

The two implicit items in the original openFile were the current user permissions and the filesystem itself.
This "pure" variant promises to return the Handle.
But, just looking at this, we can see that there are two possible reasons why we might not get a file back.
The filepath might be wrong.
And we might not have permissions for some part of the directory tree.
These are entirely reasonable errors that we might expect to occur in this pure version of the code.
Since they'd occur even in the pure version, that suggests to me that they should not be exceptions.


# Consider purity

```haskell
data OpenFileError
    = FileDoesNotExist FilePath
    | PermissionDenied

openFile 
    :: FilePath 
    -> IOMode
    -> IO (Either OpenFileError Handle)
```

Note:

This function has used our insight from the "purified" version to provide the most common error cases to the user.
We can still throw some really weird exceptions in `IO`, which is fine, but the common case is covered here.


# Don't Throw Pure Errors

```haskell
import GHC.TypeLits

data OpenFileError
    = FileDoesNotExist FilePath
    | PermissionDenied

instance (TypeError ('Text "this is pure!"))
    => Exception OpenFileError where
```

Note:

Don't throw pure errors.
If we can throw a pure error, then that means we have many possible ways of handling these cases.
An `IO (Either OpenFileError a)` might throw the error via IO, or it might return it via the Either.
If you're 100% sure that you don't ever want this to be thrown in the runtime, you can use GHC's custom type error stuff to make an instance that can't ever be used.
This helps us to narrow down *where* exceptions can come from.


# Only One Constructor

## Bad

```haskell
data ArithException
    = Overflow
    | Underflow
    | LossOfPrecision
    | DivideByZero
    | Denormal
    | RatioZeroDenominator
```

Note:

Runtime exceptions should only have one constructor.
The constructor must match the name of the type.
This makes it much easier to know *what* threw the exception, and it makes it easier to find info about the exception.


# Only One Constructor

## Good

```haskell
data Overflow             = Overflow
data Underflow            = Underflow
data LossOfPrecision      = LossOfPrecision
data DivideByZero         = DivideByZero
data Denormal             = Denormal
data RatioZeroDenominator = RatioZeroDenominator
```

Note:

Now, we have separated out all of our exceptions into single constructor types.
This makes sense.
After all, addition can't cause a DivideByZero exception, so why would it be in the same type?
If I want to catch an Overflow exception, then I can do that and ignore any potential DivideByZero exception as well.


# Debug Payload

Include enough information in the exception to write a unit test if it is thrown.

Note:

When you write an exception type, you are expressing that something is so weird and wrong that you have no idea how to handle it.
This means that you should include useful info in the payload.
This is probably one of the most important things in my talk.


# Debug Payload

## Bad

```haskell
recv: resource vanished (Connection reset by peer)
```

Note:

Raise your hand if you've debugged this guy before.
Your program is chugging along fine, until all of the sudden, this exception hits your screen and that's it.
No context, no information, no help.
It's awful!
We're going to be stuck investigating this a while using boring old school methods like "reading code" and "interactive debugging"


# Debug Payload

## Still bad

```haskell
data CsvParseFailure = CsvParseFailure Text

main =
    throw $ CsvParseFailure "it just ain't right"
```

Note:

When I say "lots of information", I don't mean text.
You should really resist the urge to use text as a payload in your errors.
It provides nearly no information and makes it extremely difficult to *handle* your error when caught.


# Debug Payload

## Good

```haskell
data CsvParseFailure = CsvParseFailure
    { headers :: [ByteString]
    , row     :: [ByteString]
    , failed  :: Maybe ByteString
    , message :: Text
    }
```

Note:

Now this exception type is good.
If we're trying to deserialize some CSV data, and we get an unexpected failure, we want to be able to write tests and fix it.
This gives us the headers of the CSV file, the row that failed, the *item* that failed, and the message from the CSV library why it failed.
We can plug the data from this exception into a unit test and fix our code in a jiffy.


# In short:

An exception is sending a message to yourself in the future.

Include enough information that it will be a useful message!

Note:

You want an exception to be extremely specific, rare, and contain a lot of useful information so you can fix it easily.
