Preface
=======

**The Philosophy**

The Smarty design was largely driven by these goals:

-   clean separation of presentation from application code

-   PHP backend, Smarty template frontend

-   complement PHP, not replace it

-   fast development/deployment for programmers and designers

-   quick and easy to maintain

-   syntax easy to understand, no PHP knowledge necessary

-   flexibility for custom development

-   security: insulation from PHP

-   free, open source

**What is Smarty?**

Smarty is a template engine for PHP, facilitating the separation of
presentation (HTML/CSS) from application logic. This implies that *PHP
code is application logic*, and is separated from the presentation.

**Two camps of thought**

When it comes to templating in PHP, there are basically two camps of
thought. The first camp exclaims that \"PHP is a template engine\". This
approach simply mixes PHP code with HTML. Although this approach is
fastest from a pure script-execution point of view, many would argue
that the PHP syntax is messy and complicated when mixed with tagged
markup such as HTML.

The second camp exclaims that presentation should be void of all
programming code, and instead use simple tags to indicate where
application content is revealed. This approach is common with other
template engines (even in other programming languages), and is also the
approach that Smarty takes. The idea is to keep the templates focused
squarely on presentation, void of application code, and with as little
overhead as possible.

**Why is separating PHP from templates important?**

Two major benefits:

-   SYNTAX: Templates typically consist of semantic markup such as HTML.
    PHP syntax works well for application code, but quickly degenerates
    when mixed with HTML. Smarty\'s simple {tag} syntax is designed
    specifically to express presentation. Smarty focuses your templates
    on presentation and less on \"code\". This lends to quicker template
    deployment and easier maintenance. Smarty syntax requires no working
    knowledge of PHP, and is intuitive for programmers and
    non-programmers alike.

-   INSULATION: When PHP is mixed with templates, there are no
    restrictions on what type of logic can be injected into a template.
    Smarty insulates the templates from PHP, creating a controlled
    separation of presentation from business logic. Smarty also has
    security features that can further enforce restrictions on
    templates.

**Web designers and PHP**

A common question: \"Web designers have to learn a syntax anyways, why
not PHP?\" Of course web designers can learn PHP, and they may already
be familiar with it. The issue isn\'t their ability to learn PHP, it is
about the consequences of mixing PHP with HTML. If designers use PHP, it
is too easy to add code into templates that doesn\'t belong there (you
just handed them a swiss-army knife when they just needed a knife.) You
can teach them the rules of application design, but this is probably
something they don\'t really need to learn (now they are developers!)
The PHP manual is also an overwhelming pile of information to sift
through. It is like handing the owner of a car the factory assembly
manual when all they need is the owners manual. Smarty gives web
designers exactly the tools they need, and gives developers fine-grained
control over those tools. The simplicity of the tag-based syntax is also
a huge welcome for designers, it helps them streamline the organization
and management of templates.

**Implementation is Important**

Although Smarty gives you the tools to make a clean separation of
presentation from application code, it also gives you plenty of room to
bend those rules. A poor implementation (i.e. injecting PHP in
templates) will cause more problems than the presentation separation was
meant to resolve. The documentation does a good job of indicating what
things to watch out for. Also see the Best Practices section of the
Smarty website.

**How does it work?**

Under the hood, Smarty \"compiles\" (basically copies and converts) the
templates into PHP scripts. This happens once when each template is
first invoked, and then the compiled versions are used from that point
forward. Smarty takes care of this for you, so the template designer
just edits the Smarty templates and never has to manage the compiled
versions. This approach keeps the templates easy to maintain, and yet
keeps execution times extremely fast since the compiled code is just
PHP. And of course, all PHP scripts take advantage of PHP op-code caches
such as APC.

**Template Inheritance**

Template inheritance is new to Smarty 3, and it\'s one of many great new
features. Before template inheritance, we managed our templates in
pieces such as header and footer templates. This organization lends
itself to many problems that require some hoop-jumping, such as managing
content within the header/footer on a per-page basis. With template
inheritance, instead of including other templates we maintain our
templates as single pages. We can then manipulate blocks of content
within by inheriting them. This makes templates intuitive, efficient and
easy to manage. See the Template Inheritance section of th Smarty
website for more info.

**Why not use XML/XSLT syntax?**

There are a couple of good reasons. First, Smarty can be used for more
than just XML/HTML based templates, such as generating emails,
javascript, CSV, and PDF documents. Second, XML/XSLT syntax is even more
verbose and fragile than PHP code! It is perfect for computers, but
horrible for humans. Smarty is about being easy to read, understand and
maintain.

**Template Security**

Although Smarty insulates you from PHP, you still have the option to use
it in certain ways if you wish. Template security forces the restriction
of PHP (and select Smarty functions.) This is useful if you have third
parties editing templates, and you don\'t want to unleash the full power
of PHP or Smarty to them.

**Integration**

Sometimes Smarty gets compared to Model-View-Controller (MVC)
frameworks. Smarty is not an MVC, it is just the presentation layer,
much like the View (V) part of an MVC. As a matter of fact, Smarty can
easily be integrated as the view layer of an MVC. Many of the more
popular ones have integration instructions for Smarty, or you may find
some help here in the forums and documentation.

**Other Template Engines**

Smarty is not the only engine following the *\"Separate Programming Code
from Presentation\"* philosophy. For instance, Python has template
engines built around the same principles such as Django Templates and
CheetahTemplate. *Note: Languages such as Python do not mix with HTML
natively, which give them the advantage of proper programming code
separation from the outset. There are libraries available to mix Python
with HTML, but they are typically avoided.*

**What Smarty is Not**

Smarty is not an application development framework. Smarty is not an
MVC. Smarty is not an alternative to Zend Framework, CodeIgniter,
PHPCake, or any of the other application development frameworks for PHP.

Smarty is a template engine, and works as the (V)iew component of your
application. Smarty can easily be coupled to any of the engines listed
above as the view component. No different than any other software,
Smarty has a learning curve. Smarty does not guarantee good application
design or proper separation of presentation, this still needs to be
addressed by a competent developer and web designer.

**Is Smarty Right for Me?**

Smarty is not meant to be a tool for every job. The important thing is
to identify if Smarty fits your needs. There are some important
questions to ask yourself:

TEMPLATE SYNTAX. Are you content with PHP tags mixed with HTML? Are your
web designers comfortable with PHP? Would your web designers prefer a
tag-based syntax designed for presentation? Some experience working with
both Smarty and PHP helps answer these questions.

THE BUSINESS CASE: Is there a requirement to insulate the templates from
PHP? Do you have untrusted parties editing templates that you do not
wish to unleash the power of PHP to? Do you need to programmatically
control what is and is not available within the templates? Smarty
supplies these capabilities by design.

FEATURE SET: Does Smarty\'s features such as caching, template
inheritance and plugin architecture save development cycles writing code
that would be needed otherwise? Does the codebase or framework you plan
on using have the features you need for the presentation component?

Templating in PHP is a hot topic, and opinions widely vary. It is
important that you understand Smarty, understand your own requirements,
and make an informed decision for yourself. You are welcome to ask
specific questions in the forums or the IRC channel.

See also the section about \"Use Cases and Work Flow\" on the Smarty
website.

**Sites using Smarty**

There are tens of thousands of unique visitors on the Smarty website
daily, mostly developers reading documentation. Many well-known PHP
projects make use of Smarty such as XOOPS CMS, CMS Made Simple , Tiki
CMS/Groupware and X-Cart to name a few.

**Summary**

Whether you are using Smarty for a small website or massive enterprise
solution, it can accommodate your needs. There are numerous features
that make Smarty a great choice:

-   separation of PHP from HTML/CSS just makes sense

-   readability for organization and management

-   security for 3rd party template access

-   feature completeness, and easily extendable to your own needs

-   massive user base, Smarty is here to stay

-   LGPL license for commercial use

-   100% free to use, open source project
