</head>
<body lang="sv-SE" text="#00000a" bgcolor="#ffffff" dir="ltr">
<div title="header">
	<p class="western" style="margin-bottom: 1.27cm; font-style: normal">
	<h1>JWithATwist Mock Parser</h1>
	</p>
</div>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<span lang="sv-SE">JWithATwist is a </span><span lang="sv-SE">programming
language </span><span lang="sv-SE">under development. It </span><span lang="sv-SE">has</span><span lang="sv-SE">
many</span><span lang="sv-SE"> </span><span lang="sv-SE">similarities
with</span><span lang="sv-SE"> </span><span lang="sv-SE">J.</span><span lang="sv-SE">
</span>You can look at www.jsoftware.com to find out what J is. 
</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
This parser is supposed to become the parser for this new language,
but the language elements are only mock objects. You can only work
with integers. There are only a few operations you can use.</p>
<ul>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	I use the J syntax to describe this language.</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	NOUN - an n-dimensional array of values where the values are simple
	data types like character or integer, but they can also can be other
	n-dimensional arrays. 
	</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	VERB - a function with one or two noun arguments returning a noun.</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	ADVERB - an operator with a verb argument and one or two noun
	arguments, usually returning a verb.</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	CONJUNCTION - an operator with two verb arguments and one or two
	noun arguments, normally returning a verb.</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	A MONADIC VERB has one argument, a DYADIC VERB has two. Adverbs and
	conjunctions are also called monadic or dyadic after their number of
	noun argments.</p>
</ul>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
The operations in this mock parser:</p>
<ul>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	<span lang="sv-SE">Dyadic verb </span><span lang="sv-SE">A</span><span lang="sv-SE">ddition,
	+</span></p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	<span lang="sv-SE">Dyadic verb </span><span lang="sv-SE">Subtraction,
	-</span></p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	<span lang="sv-SE">Monadic verb </span><span lang="sv-SE">Negation,
	|-</span></p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	<span lang="sv-SE">Monadic adverb, / . This operator is defined u /
	y &lt;-&gt; y u y. It applies the in</span><span lang="sv-SE">-</span><span lang="sv-SE">fix
	u verb between</span><span lang="sv-SE"> </span><span lang="sv-SE">two
	copies of the verb argument. </span><span lang="sv-SE"> </span><span lang="sv-SE">+
	/ 1 &lt;-&gt; 1 + 1 &lt;-&gt; 2.</span></p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	<span lang="sv-SE">Dyadic adverb, /- . This operator is defined x
	u/</span><span lang="sv-SE">-</span><span lang="sv-SE"> y &lt;-&gt;
	x u y. It applies the</span><span lang="sv-SE">  </span><span lang="sv-SE">infix
	u verb between</span><span lang="sv-SE"> </span><span lang="sv-SE">the
	left and right argument. </span><span lang="sv-SE"> </span><span lang="sv-SE">1
	+ /- 1 &lt;-&gt; 1 + 1 &lt;-&gt; 2</span></p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	<span lang="sv-SE">Monadic conjunction, |. . This operator is
	defined  u |. v y &lt;-&gt; u y v y. It applies the infix dyadic</span><span lang="sv-SE">
	</span><span lang="sv-SE">verb v between copies of the right
	argument and the monadic</span><span lang="sv-SE"> </span><span lang="sv-SE">verb
	u to the result.</span><span lang="sv-SE"> </span><span lang="sv-SE">|-
	|. + </span><span lang="sv-SE">1</span><span lang="sv-SE"> &lt;-&gt;
	|- </span><span lang="sv-SE">1</span><span lang="sv-SE"> </span><span lang="sv-SE">+
	</span><span lang="sv-SE">1</span><span lang="sv-SE"> &lt;-&gt; -</span><span lang="sv-SE">2
	</span><span lang="sv-SE">.</span></p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	<span lang="sv-SE">Dyadic conjunction, .</span><span lang="sv-SE"> </span><span lang="sv-SE">
	. This operator is defined x u . v y &lt;-&gt; u x v y</span><span lang="sv-SE">
	</span><span lang="sv-SE">. It ap</span><span lang="sv-SE">p</span><span lang="sv-SE">lies
	the infix dyadic</span><span lang="sv-SE"> </span><span lang="sv-SE">verb</span><span lang="sv-SE">
	v</span><span lang="sv-SE"> between the left and right arguments and
	the monadic</span><span lang="sv-SE"> </span><span lang="sv-SE">verb
	u to the result. 2 |- . + 5 </span><span lang="sv-SE"> &lt;</span><span lang="sv-SE">-&gt;
	|- 2</span><span lang="sv-SE"> </span><span lang="sv-SE">+ 5 &lt;-&gt;
	-</span><span lang="sv-SE">7 </span><span lang="sv-SE">.</span></p>
</ul>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
There is no order of precedence between the operations. Execution is
from right to left modified by parenthesis. |- |- |- 1 &lt;-&gt; -1</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
The language elements are associated to it's arguments like this.</p>
<ul>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	A monadic verb has one noun argument to the right. Like this: |- 1 .</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	A dyadic verb has one noun argument to the left and one to the
	right. Like this: 1 + 1 .</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	A monadic adverb has a verb argument to the left and a noun argument
	to the right. Like this: + / 1 .</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	A dyadic adverb has a verb argument to the left, one noun argument
	to the left and one to the right. Like this: 1 + /- 1 .</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	A monadic conjunction has a verb argument to the left and one to the
	right, and one noun argument to the right. Like this: |- |. + 1 .</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	A dyadic conjunction has a verb argument to the left and one to the
	right, and one noun argument to the left and one to the right. Like
	this: 1 |- . + 1</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	An adverb to the left of a conjunction is together with it's verb
	the left argument of the conjunction. Like this in the monadic case:
	+ / |. + 1 . Like this in the dyadic case: 1 + / . + 1 .</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	An adverb together with its left verb forms a new verb. A
	conjunction together with its right verb and it's left verb or
	adverb forms a new verb.</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	Monadic verbs, adverbs and conjunctions create verb trains like
	this: |-        + /        |-        |- |. +        |-        + / 
	|. +        |-        1 
	</p>
</ul>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
I use extra blanks to show the separation between verbs.</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
You always need at least one blank between two syntax elements.</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
A program is called a NOUN DEFINITION. It is written between curly
brackets, like this 
</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
{ <i>some code</i> } It can contain VERB DEFINITIONS, written between
curly brackets where the</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
first bracket is immediately followed by ! . Like this {! <i>some
code </i>} .</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
A verb definition can contain the <span style="font-style: normal">NOUN
FUNCTIONS</span><i> -</i><span style="font-style: normal"> </span><span style="font-style: normal">LEFT
NOUN</span><span style="font-style: normal"> and </span><span style="font-style: normal">RIGHT
NOUN -</span><span style="font-style: normal"> denoted by</span><span style="font-style: normal">
</span><span style="font-style: normal">[</span><span style="font-style: normal">
</span><span style="font-style: normal">and</span><span style="font-style: normal">
</span><span style="font-style: normal">] . The Left</span><span style="font-style: normal">
</span><span style="font-style: normal">Noun is a placeholder for the
noun immediately preceding the verb</span><span style="font-style: normal">
</span><span style="font-style: normal">defi</span><span style="font-style: normal">-
</span><span style="font-style: normal">nition.</span><span style="font-style: normal">
</span><span style="font-style: normal">The Right Noun is a</span><span style="font-style: normal">
</span><span style="font-style: normal">placeholder for the noun
immediately following the verb definition.</span></p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
A verb definition containing only Right Nouns forms a monadic verb. A
verb definition containing a Left Noun forms a dyadic verb, even if
there is no Right Noun. In that case you are forced to have a noun to
the right of the verb definition and this noun is silently ignored.</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
This is a verb definition of a monadic increment verb {! 1 + ] } .
Example of use: {! 1 + ] } 2 .</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
This is a verb definition of a dyadic addition verb {! [ + ] } .
Example of use : 1 {! [ + ] } 2 .</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
Left and Right nouns within brackets are placeholders for the noun
arguments to the first verb definition enclosing the brackets.  In
this example the Left noun is one and the Right noun is two: 1 {! ( [
+ ] ) + 1 } 2</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
There is no way to define adverbs and conjunctions in this mock
parser. 
</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<span style="font-style: normal">T</span><span style="font-style: normal">he
verb definitions are very similar to the </span><span style="font-style: normal">DIRECT
DEFINITION FORM</span><span style="font-style: normal"> Ken Iverson
used at his</span><span style="font-style: normal"> </span><span style="font-style: normal">lectures,
see <a class="western" href="http://tinyurl.com/npx2umk">http://tinyurl.com/npx2umk</a>
.</span></p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
In the present implementation the noun definitions does not add any
functional value.</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<span style="font-style: normal">I will use this parser as an
argument in a discussion about the J language. I have questioned the
syntax of</span><span style="font-style: normal"> </span><span style="font-style: normal">tacit
J many times. The answer is always - </span><span style="font-style: normal">H</span><span style="font-style: normal">ow
should it be instead. This</span><span style="font-style: normal"> </span><span style="font-style: normal">is
</span><span style="font-style: normal">how I think it should be.</span><span style="font-style: normal">
Nearly exactly like normal scripted J.</span><span style="font-style: normal">
I want to add that if</span><span style="font-style: normal"> </span><span style="font-style: normal">you
add a Left verb and a Right verb to this parser, you could add a verb
definition from verbs which would possibly let you use higher order
functions on these verbs.</span></p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
You could of course also use Left verb and Right verb to define
adverbs and conjunctions in some kind of direct definition form.</p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<span style="font-style: normal">The</span><span style="font-style: normal">
tacit J</span><span style="font-style: normal"> parser</span><span style="font-style: normal">
is in</span><span style="font-style: normal"> many ways similar</span><span style="font-style: normal">
to</span><span style="font-style: normal"> </span><span style="font-style: normal">this
</span><span style="font-style: normal">parser</span><span style="font-style: normal">.</span><span style="font-style: normal">
It</span><span style="font-style: normal"> does not have definitions
o</span><span style="font-style: normal">f </span><span style="font-style: normal">adverbs
and conjunctions</span><span style="font-style: normal"> either.</span></p>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
Some notes on implementation:</p>
<ul>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	The parser creates a composite compiled .NET function which is
	executed when the parsing process is finished. It is more like
	JIT-compiled than interpreted code.</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	The parser is written in F# and with #FParsec.</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	In the F# environment you can write code nearly as efficient as in
	C++.</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	The full .NET environment and the rest of the Windows environment,
	called Windows RT, is easily available from F#. Integration with the
	Windows environment is easy.</p>
	<li/>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
	In JWithATwist an adverb or conjunction always returns a noun.</p>
</ul>
<p class="western" style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<span style="font-style: normal">A</span><span style="font-style: normal">
sample session :</span></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ 1 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">1</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ 1 + 1 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">2</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ ( 1 - 1 ) + 1 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">1</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ ( 1 - 1 ) {! [ + ] } 1 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">1</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ { 1 - 1 } {! [ + ] } 1 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">1</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ + / 1 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">2</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ {! [ + ] } / 1 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">2</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ + / |. + 1 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">4</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ 2 + / . + 1 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">6</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ 2 + / . + |- |- 1 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">6</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ 2 + / . {! [ + ] } |- |- 1 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">6</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ 2 + / . + |- {! |- ] } 1 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">6</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ 2 + /- 3 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">5	</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ 5 {! ( [ + ] ) + [ + ] } 7 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">24</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">{ |- + / |- |. + + / |. + |- 5 }</font></p>
<p style="margin-bottom: 0.11cm; font-style: normal; line-height: 100%">
<font face="DejaVu Sans, sans-serif">-80</font></p>

</body>
</html>
