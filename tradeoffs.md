# Tradeoffs

Note:

So, we've seen a bit of a whirlwind tour of exceptions and errors thus far.
We've talked about how Haskell represents them semantically, how Java compares, and how exception-less languages handle error cases.
At this point, the talk is going to get a little squishier.
I'm going to talk more about my experiences and philosophy about how to handle exceptions and error cases, and less about objective facts about the world.

Up to this point, I've mostly talked about Haskell's runtime exceptions.
I'm going to use exceptions now to mean *any* kind of short-circuiting error value, along with the relevant means of catching them.


# Ergonomics 
# vs 
# Safety

Note:

This is a really big one.
I mentioned this earlier, but Java has a feature called Checked Exceptions.
I kinda like it.
Every method that throws an exception must annotate this.
In a way, the set of methods that a method can throw becomes part of the type, or signature, of the method.
Java is more type-safe than Haskell when it comes to exceptions.

Now, almost everyone that I've ever talked to hates checked exceptions.
The ergonomics are bad, and in the few Java codebases I've worked on, the common practice was to catch checked exceptions and rethrow as unchecked exceptions.
The lesson I take from this is that: if we try to give too much safety and sacrifice ergonomics, we'll end up sacrificing safety too.


# Brevity 
# vs 
# Precision 

Note: 

This is a similar dimension.
We want to be very precise about what errors we can throw.
However, this tends to give a decent amount of boilerplate, and we hate boilerplate.
So we make the code more concise, but this reduces precision in many cases.

We can make the code easier to write, but this makes it harder to analyze and work with.


# Composability
# vs 
# Specificity 

Note:

We want to have the most specific error types possible.
But we also want seamless composition between functions that have different error types.
These desires are at odds with each other, as well.
Unfortunately, Haskell doesn't really have a good way of making this work.
When you `bind` with Either or ExceptT, the type of the exception must be the same on both sides.
And the `MonadError` type class doesn't allow you to change the error type at all.
Broadly speaking, all of these tradeoffs are the tension between unchecked IO exceptions and checked exceptions, however we check them.
