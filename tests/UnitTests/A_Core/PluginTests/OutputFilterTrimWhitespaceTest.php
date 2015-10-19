<?php
/**
 * Smarty PHPunit tests for PHP resources
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for PHP resource tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class OutputFilterTrimWhitespaceTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->loadFilter('output', 'trimwhitespace');
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    public function testWhitespace()
    {
        $expected =
'<!DOCTYPE html> <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="de" lang="de"> <head> <meta charset="utf-8"/> <meta http-equiv="content-type" content="text/html; charset=utf-8"/> <title>whitespace</title> <meta name="title" content=""/> <meta name="description" content=""/> <link rel="stylesheet" type="text/css" href="screen.css"/> </head> <body> <!--[if lte IE 6]>internet explorer conditional comment<![endif]--> <!--[if lte IE 7]>internet explorer conditional comment<![endif]--> <div class="  asdasd   " id=\'not\' data-one=" " style=" " title=\' \'></div> <img src="foo" alt=""/> <script type="text/javascript">
    foobar
</script> <script>
    foobar
</script> <pre id="foobar">
        foobar
    </pre> <pre>
        foobar
    </pre> <p> <textarea name="foobar">
        foobar
    </textarea> </p> </body> </html>';


        $this->assertEquals($this->normalizeString($expected), $this->normalizeString($this->smarty->fetch('whitespace.tpl')));
    }
}
