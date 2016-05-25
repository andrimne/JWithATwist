<html xmlns="http://www.w3.org/1999/xhtml">
<head>
	<meta charset="ISO-8859-1" />
	<title></title>
</head>
<body dir="ltr">
<h1>JWithATwist Mock Parser</h1>

<p>JWithATwist is a programming language under development. It has many similarities with J. You can look at <a href="http://www.jsoftware.com">www.jsoftware.com</a> to find out what J is. This parser is supposed to become the parser for this new language, but the language elements are only mock objects. You can only work with integers. There are only a few operations you can use.</p>

<p>I use the J syntax to describe this language.</p>

<ul>
	<li>NOUN - an n-dimensional array of values where the values are simple data types like character or integer, but they can also be other n-dimensional arrays.</li>
	<li>VERB - a function with one or two noun arguments returning a noun.</li>
	<li>ADVERB - an operator with a verb argument and one or two noun arguments, usually returning a verb.</li>
	<li>CONJUNCTION - an operator with two verb arguments and one or two noun arguments, normally returning a verb.</li>
	<li>A MONADIC VERB has one argument, a DYADIC VERB has two. Adverbs and conjunctions are also called monadic or dyadic after their number of noun argments.</li>
</ul>

<p>The operations in this mock parser:</p>

<ul>
	<li>Dyadic verb Addition,<code> +</code></li>
	<li>Dyadic verb Subtraction,<code> - </code></li>
	<li>Monadic verb Negation,<code> |- </code></li>
	<li>Monadic adverb,<code> / </code>. This operator is defined <code> u / y &lt;-&gt; y u y</code>. It applies the in-fix u verb between two copies of the verb argument.<code> + / 1 &lt;-&gt; 1 + 1 &lt;-&gt; 2</code>.</li>
	<li>Dyadic adverb,<code> /- </code>. This operator is defined <code>x u/- y &lt;-&gt; x u y</code>. It applies the infix u verb between the left and right argument.<code> 1 + /- 1 &lt;-&gt; 1 + 1 &lt;-&gt; 2</code></li>
	<li>Monadic conjunction,<code> |. </code>. This operator is defined<code> u |. v y &lt;-&gt; u y v y</code>. It applies the infix dyadic verb v between copies of the right argument and the monadic verb u to the result.<code> |- |. + 1 &lt;-&gt; |- 1 + 1 &lt;-&gt; -2 </code>.</li>
	<li>Dyadic conjunction,<code> . </code>. This operator is defined<code> x u . v y &lt;-&gt; u x v y </code>. It applies the infix dyadic verb v between the left and right arguments and the monadic verb u to the result.<code> 2 |- . + 5 &lt;-&gt; |- 2 + 5 &lt;-&gt; -7 </code>.</li>
</ul>

<p>There is no order of precedence between the operations. Execution is from right to left modified by parenthesis.<code> |- |- |- 1 &lt;-&gt; -1</code></p>

<p>The language elements are associated to it&#39;s arguments like this:</p>

<ul>
	<li>A monadic verb has one noun argument to the right. Like this: <code>|- 1 </code>.</li>
	<li>A dyadic verb has one noun argument to the left and one to the right. Like this:<code> 1 + 1 </code>.</li>
	<li>A monadic adverb has a verb argument to the left and a noun argument to the right. Like this:<code> + / 1 </code>.</li>
	<li>A dyadic adverb has a verb argument to the left, one noun argument to the left and one to the right. Like this:<code> 1 + /- 1 </code>.</li>
	<li>A monadic conjunction has a verb argument to the left and one to the right, and one noun argument to the right. Like this: <code>|- |. + 1 </code>.</li>
	<li>A dyadic conjunction has a verb argument to the left and one to the right, and one noun argument to the left and one to the right. Like this: <code>1 |- . + 1</code></li>
	<li>An adverb to the left of a conjunction is together with it&#39;s verb the left argument of the conjunction. Like this in the monadic case:<code> + / |. + 1 </code>. Like this in the dyadic case:<code> 1 + / . + 1 </code>.</li>
</ul>	
<p>An adverb together with its left verb forms a new verb. A conjunction together with its right verb and it&#39;s left verb or adverb forms a new verb.</p>

<p>Monadic verbs, adverbs and conjunctions create verb trains like this:</p>

<pre><code> |-     + /     |-     |- |. +     |-     + / |. +     |- 1</code></pre>

<p>I use extra blanks to show the separation between verbs.</p>

<p>You always need at least one blank between two syntax elements.</p>

<p>A program is called a NOUN DEFINITION. It is written between curly brackets, like this <code>{ some code }</code> It can contain VERB DEFINITIONS, written between curly brackets where the first bracket is immediately followed by ! . Like this <code>{! some code }</code> .</p>

<p>A verb definition can contain the NOUN FUNCTIONS - LEFT NOUN and RIGHT NOUN - denoted by [ and ] . The Left Noun is a placeholder for the noun immediately preceding the verb definition. The Right Noun is a placeholder for the noun immediately following the verb definition.</p>

<p>A verb definition containing only Right Nouns forms a monadic verb. A verb definition containing a Left Noun forms a dyadic verb, even if there is no Right Noun. In that case you are forced to have a noun to the right of the verb definition and this noun is silently ignored.</p>

<p>This is a verb definition of a monadic increment verb<code> {! 1 + ] } </code>. Example of use:<code> {! 1 + ] } 2</code> .</p>

<p>This is a verb definition of a dyadic addition verb <code>{! [ + ] } </code>. Example of use :<code> 1 {! [ + ] } 2 </code>.</p>

<p>Left and Right nouns within brackets are placeholders for the noun arguments to the first verb definition enclosing the brackets. In this example the Left noun is one and the Right noun is two: <code>1 {! ( [ + ] ) + 1 } 2</code></p>

<p>There is no way to define adverbs and conjunctions in this mock parser.</p>

<p>The verb definitions are very similar to the DIRECT DEFINITION FORM Ken Iverson used at his lectures, see <a href="http://tinyurl.com/npx2umk">http://tinyurl.com/npx2umk</a> .</p>

<p>In the present implementation the noun definitions does not add any functional value.</p>

<p>I will use this parser as an argument in a discussion about the J language. I have questioned the syntax of tacit J many times. The answer is always - How should it be instead. This is how I think it should be. Nearly exactly like normal scripted J.</p> 
<p> I want to add that if you add a Left verb and a Right verb to this parser, you could add a definition of a new functional entity with one or two verb arguments and a verb result which would possibly let you use higher order functions on these verbs.</p>

<p>You could of course also use Left verb and Right verb to define adverbs and conjunctions in some kind of direct definition form.</p>

<p>The tacit J parser is in many ways similar to this parser. It does not have definitions of adverbs and conjunctions either.</p>

<p>Some notes on implementation:</p>

<ul>
	<li>The parser creates a composite compiled .NET function which is executed when the parsing process is finished. It is more like JIT-compiled than interpreted code.</li>
	<li>The parser is written in F# and with FParsec.</li>
	<li>In the F# environment you can write code nearly as efficient as in C++.</li>
	<li>The full .NET environment and the rest of the Windows environment, called Windows RT, is easily available from F#. Integration with the Windows environment is easy.</li>
	<li>In JWithATwist an adverb or conjunction always returns a noun.</li>
</ul>

<p>A sample session :<br />
<code>{ 1 }</code><br />
<code>1</code><br />
<code>{ 1 + 1 }</code><br />
<code>2</code><br />
<code>{ ( 1 - 1 ) + 1 }</code><br />
<code>1</code><br />
<code>{ ( 1 - 1 ) {! [ + ] } 1 }</code><br />
<code>1</code><br />
<code>{ { 1 - 1 } {! [ + ] } 1 }</code><br />
<code>1</code><br />
<code>{ + / 1 } </code><br />
<code>2</code><br />
<code>{ {! [ + ] } / 1 }</code><br />
<code>2</code><br />
<code>{ + / |. + 1 }</code><br />
<code>4</code><br />
<code>{ 2 + / . + 1 }</code><br />
<code>6</code><br />
<code>{ 2 + / . + |- |- 1 }</code><br />
<code>6</code><br />
<code>{ 2 + / . {! [ + ] } |- |- 1 }</code><br />
<code>6</code><br />
<code>{ 2 + / . + |- {! |- ] } 1 }</code><br />
<code>6</code><br />
<code>{ 2 + /- 3 }</code><br />
<code>5</code><br />
<code>{ 5 {! ( [ + ] ) + [ + ] } 7 }</code><br />
<code>24</code><br />
<code>{ |- + / |- |. + + / |. + |- 5 }</code><br />
<code>-80</code></p>
</body>
</html>
