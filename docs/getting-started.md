What is Smarty?
===============

Smarty is a template engine for PHP. More specifically, it facilitates a
manageable way to separate application logic and content from its
presentation. This is best described in a situation where the
application programmer and the template designer play different roles,
or in most cases are not the same person.

For example, let\'s say you are creating a web page that is displaying a
newspaper article.

-   The article `$headline`, `$tagline`, `$author` and `$body` are
    content elements, they contain no information about how they will be
    presented. They are [passed](#api.assign) into Smarty by the
    application.

-   Then the template designer edits the templates and uses a
    combination of HTML tags and [template tags](#language.basic.syntax)
    to format the presentation of these
    [variables](#language.syntax.variables) with elements such as
    tables, div\'s, background colors, font sizes, style sheets, svg
    etc.

-   One day the programmer needs to change the way the article content
    is retrieved, ie a change in application logic. This change does not
    affect the template designer, the content will still arrive in the
    template exactly the same.

-   Likewise, if the template designer wants to completely redesign the
    templates, this would require no change to the application logic.

-   Therefore, the programmer can make changes to the application logic
    without the need to restructure templates, and the template designer
    can make changes to templates without breaking application logic.

One design goal of Smarty is the separation of business logic and
presentation logic.

-   This means templates can certainly contain logic under the condition
    that it is for presentation only. Things such as
    [including](#language.function.include) other templates,
    [alternating](#language.function.cycle) table row colors,
    [upper-casing](#language.modifier.upper) a variable,
    [looping](#language.function.foreach) over an array of data and
    [displaying](#api.display) it are examples of presentation logic.

-   This does not mean however that Smarty forces a separation of
    business and presentation logic. Smarty has no knowledge of which is
    which, so placing business logic in the template is your own doing.

-   Also, if you desire *no* logic in your templates you certainly can
    do so by boiling the content down to text and variables only.

**Some of Smarty\'s features:**

-   It is extremely fast.

-   It is efficient since the PHP parser does the dirty work.

-   No template parsing overhead, only compiles once.

-   It is smart about [recompiling](#variable.compile.check) only the
    template files that have changed.

-   You can easily create your own custom
    [functions](#language.custom.functions) and [variable
    modifiers](#language.modifiers), so the template language is
    extremely extensible.

-   Configurable template [{delimiter}](#variable.left.delimiter) tag
    syntax, so you can use `{$foo}`, `{{$foo}}`, `<!--{$foo}-->`, etc.

-   The [`{if}..{elseif}..{else}..{/if}`](#language.function.if)
    constructs are passed to the PHP parser, so the `{if...}` expression
    syntax can be as simple or as complex an evaluation as you like.

-   Allows unlimited nesting of
    [`sections`](#language.function.section), `if's` etc.

-   Built-in [caching](#caching) support

-   Arbitrary [template](#resources) sources

-   [Template Inheritance](#advanced.features.template.inheritance) for
    easy management of template content.

-   [Plugin](#plugins) architecture

Installation
============

Requirements {#installation.requirements}
============

Smarty requires a web server running PHP 5.2 or greater.

Basic Installation {#installing.smarty.basic}
==================

Install the Smarty library files which are in the `/libs/` sub directory
of the distribution. These are `.php` files that you SHOULD NOT edit.
They are shared among all applications and only get changed when you
upgrade to a new version of Smarty.

In the examples below the Smarty tarball has been unpacked to:

-   `/usr/local/lib/Smarty-v.e.r/` for \*nix machines

-   and `c:\webroot\libs\Smarty-v.e.r\` for the windows environment.

<!-- -->


    Smarty-v.e.r/
       libs/
          Smarty.class.php
          debug.tpl
          sysplugins/* (everything)
          plugins/*    (everything)

        

Smarty uses a PHP [constant](&url.php-manual;define) named
[`SMARTY_DIR`](#constant.smarty.dir) which is the **full system file
path** to the Smarty `libs/` directory. Basically, if your application
can find the `Smarty.class.php` file, you do not need to set the
[`SMARTY_DIR`](#constant.smarty.dir) as Smarty will figure it out on its
own. Therefore, if `Smarty.class.php` is not in your
[include\_path](&url.php-manual;ini.core.php#ini.include-path), or you
do not supply an absolute path to it in your application, then you must
define `SMARTY_DIR` manually. `SMARTY_DIR` **must include a trailing
slash/**.

::: {.informalexample}
Here\'s how you create an instance of Smarty in your PHP scripts:


    <?php
    // NOTE: Smarty has a capital 'S'
    require_once('Smarty.class.php');
    $smarty = new Smarty();
    ?>

        
:::

Try running the above script. If you get an error saying the
`Smarty.class.php` file could not be found, you need to do one of the
following:


    <?php
    // *nix style (note capital 'S')
    define('SMARTY_DIR', '/usr/local/lib/Smarty-v.e.r/libs/');

    // windows style
    define('SMARTY_DIR', 'c:/webroot/libs/Smarty-v.e.r/libs/');

    // hack version example that works on both *nix and windows
    // Smarty is assumend to be in 'includes/' dir under current script
    define('SMARTY_DIR',str_replace("\\","/",getcwd()).'/includes/Smarty-v.e.r/libs/');

    require_once(SMARTY_DIR . 'Smarty.class.php');
    $smarty = new Smarty();
    ?>

        


    <?php
    // *nix style (note capital 'S')
    require_once('/usr/local/lib/Smarty-v.e.r/libs/Smarty.class.php');

    // windows style
    require_once('c:/webroot/libs/Smarty-v.e.r/libs/Smarty.class.php');

    $smarty = new Smarty();
    ?>

        


    ;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Paths and Directories ;
    ;;;;;;;;;;;;;;;;;;;;;;;;;

    ; *nix: "/path1:/path2"
    include_path = ".:/usr/share/php:/usr/local/lib/Smarty-v.e.r/libs/"

    ; Windows: "\path1;\path2"
    include_path = ".;c:\php\includes;c:\webroot\libs\Smarty-v.e.r\libs\"


    <?php
    // *nix
    ini_set('include_path', ini_get('include_path').PATH_SEPARATOR.'/usr/local/lib/Smarty-v.e.r/libs/');

    // windows
    ini_set('include_path', ini_get('include_path').PATH_SEPARATOR.'c:/webroot/lib/Smarty-v.e.r/libs/');
    ?>

        

Now that the library files are in place, it\'s time to setup the Smarty
directories for your application:

-   Smarty requires four directories which are by default named
    `templates/`, `templates_c/`, `configs/` and `cache/`

-   Each of these are definable by the Smarty class properties
    [`$template_dir`](#variable.template.dir),
    [`$compile_dir`](#variable.compile.dir),
    [`$config_dir`](#variable.config.dir), and
    [`$cache_dir`](#variable.cache.dir) respectively

-   It is highly recommended that you setup a separate set of these
    directories for each application that will use Smarty

-   You can verify if your system has the correct access rights for
    these directories with [`testInstall()`](#api.test.install).

For our installation example, we will be setting up the Smarty
environment for a guest book application. We picked an application only
for the purpose of a directory naming convention. You can use the same
environment for any application, just replace `guestbook/` with the name
of your application.


    /usr/local/lib/Smarty-v.e.r/libs/
            Smarty.class.php
            debug.tpl
            sysplugins/*
            plugins/*

    /web/www.example.com/
            guestbook/
            templates/
                index.tpl
            templates_c/
            configs/
            cache/
            htdocs/
                index.php

        

Be sure that you know the location of your web server\'s document root
as a file path. In the following examples, the document root is
`/web/www.example.com/guestbook/htdocs/`. The Smarty directories are
only accessed by the Smarty library and never accessed directly by the
web browser. Therefore to avoid any security concerns, it is recommended
(but not mandatory) to place these directories *outside* of the web
server\'s document root.

You will need as least one file under your document root, and that is
the script accessed by the web browser. We will name our script
`index.php`, and place it in a subdirectory under the document root
`/htdocs/`.

Smarty will need **write access** (windows users please ignore) to the
[`$compile_dir`](#variable.compile.dir) and
[`$cache_dir`](#variable.cache.dir) directories (`templates_c/` and
`cache/`), so be sure the web server user account can write to them.

> **Note**
>
> This is usually user "nobody" and group "nobody". For OS X users, the
> default is user "www" and group "www". If you are using Apache, you
> can look in your `httpd.conf` file to see what user and group are
> being used.


    chown nobody:nobody /web/www.example.com/guestbook/templates_c/
    chmod 770 /web/www.example.com/guestbook/templates_c/

    chown nobody:nobody /web/www.example.com/guestbook/cache/
    chmod 770 /web/www.example.com/guestbook/cache/

        

> **Note**
>
> `chmod 770` will be fairly tight security, it only allows user
> "nobody" and group "nobody" read/write access to the directories. If
> you would like to open up read access to anyone (mostly for your own
> convenience of viewing these files), you can use `775` instead.

We need to create the `index.tpl` file that Smarty will display. This
needs to be located in the [`$template_dir`](#variable.template.dir).


    {* Smarty *}

    Hello {$name}, welcome to Smarty!

        

> **Note**
>
> `{* Smarty *}` is a template [comment](#language.syntax.comments). It
> is not required, but it is good practice to start all your template
> files with this comment. It makes the file easy to recognize
> regardless of the file extension. For example, text editors could
> recognize the file and turn on special syntax highlighting.

Now lets edit `index.php`. We\'ll create an instance of Smarty,
[`assign()`](#api.assign) a template variable and
[`display()`](#api.display) the `index.tpl` file.


    <?php

    require_once(SMARTY_DIR . 'Smarty.class.php');

    $smarty = new Smarty();

    $smarty->setTemplateDir('/web/www.example.com/guestbook/templates/');
    $smarty->setCompileDir('/web/www.example.com/guestbook/templates_c/');
    $smarty->setConfigDir('/web/www.example.com/guestbook/configs/');
    $smarty->setCacheDir('/web/www.example.com/guestbook/cache/');

    $smarty->assign('name','Ned');

    //** un-comment the following line to show the debug console
    //$smarty->debugging = true;

    $smarty->display('index.tpl');

    ?>

        

> **Note**
>
> In our example, we are setting absolute paths to all of the Smarty
> directories. If `/web/www.example.com/guestbook/` is within your PHP
> include\_path, then these settings are not necessary. However, it is
> more efficient and (from experience) less error-prone to set them to
> absolute paths. This ensures that Smarty is getting files from the
> directories you intended.

Now navigate to the `index.php` file with the web browser. You should
see *\"Hello Ned, welcome to Smarty!\"*

You have completed the basic setup for Smarty!

Extended Setup {#installing.smarty.extended}
==============

This is a continuation of the [basic
installation](#installing.smarty.basic), please read that first!

A slightly more flexible way to setup Smarty is to [extend the
class](&url.php-manual;ref.classobj) and initialize your Smarty
environment. So instead of repeatedly setting directory paths, assigning
the same vars, etc., we can do that in one place.

Lets create a new directory `/php/includes/guestbook/` and make a new
file called `setup.php`. In our example environment, `/php/includes` is
in our `include_path`. Be sure you set this up too, or use absolute file
paths.


    <?php

    // load Smarty library
    require('Smarty.class.php');

    // The setup.php file is a good place to load
    // required application library files, and you
    // can do that right here. An example:
    // require('guestbook/guestbook.lib.php');

    class Smarty_GuestBook extends Smarty {

       function __construct()
       {

            // Class Constructor.
            // These automatically get set with each new instance.

            parent::__construct();

            $this->setTemplateDir('/web/www.example.com/guestbook/templates/');
            $this->setCompileDir('/web/www.example.com/guestbook/templates_c/');
            $this->setConfigDir('/web/www.example.com/guestbook/configs/');
            $this->setCacheDir('/web/www.example.com/guestbook/cache/');

            $this->caching = Smarty::CACHING_LIFETIME_CURRENT;
            $this->assign('app_name', 'Guest Book');
       }

    }
    ?>

        

Now lets alter the `index.php` file to use `setup.php`:


    <?php

    require('guestbook/setup.php');

    $smarty = new Smarty_GuestBook();

    $smarty->assign('name','Ned');

    $smarty->display('index.tpl');
    ?>

       

Now you see it is quite simple to bring up an instance of Smarty, just
use `Smarty_GuestBook()` which automatically initializes everything for
our application.
