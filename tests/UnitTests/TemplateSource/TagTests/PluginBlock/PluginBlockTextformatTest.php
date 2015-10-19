<?php
/**
 * Smarty PHPunit tests of modifier
 *
 * @package PHPunit
 * @author  Rodney Rehm
 */

/**
 * class for modifier tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class PluginBlockTextformatTest extends PHPUnit_Smarty
{
    protected $string = "\n\nThis is foo.\nThis is foo.\nThis is foo.\nThis is foo.\nThis is foo.\nThis is foo.\n\nThis is bar.\n\nbar foo bar foo     foo.\nbar foo bar foo     foo.\nbar foo bar foo     foo.\nbar foo bar foo     foo.\nbar foo bar foo     foo.\nbar foo bar foo     foo.\nbar foo bar foo     foo.\n\n";

    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testDefault()
    {
        $result = "\n\nThis is foo. This is foo. This is foo.\nThis is foo. This is foo. This is foo.\n\nThis is bar.\n\nbar foo bar foo foo. bar foo bar foo\nfoo. bar foo bar foo foo. bar foo bar\nfoo foo. bar foo bar foo foo. bar foo\nbar foo foo. bar foo bar foo foo.\n\n";
        $tpl = $this->smarty->createTemplate('eval:{textformat wrap=40}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
    }

    public function testDefaultWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = "\n\nThis is foo. This is foo. This is foo.\nThis is foo. This is foo. This is foo.\n\nThis is bar.\n\nbar foo bar foo foo. bar foo bar foo\nfoo. bar foo bar foo foo. bar foo bar\nfoo foo. bar foo bar foo foo. bar foo\nbar foo foo. bar foo bar foo foo.\n\n";
        $tpl = $this->smarty->createTemplate('eval:{textformat wrap=40}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
        Smarty::$_MBSTRING = true;
    }

    public function testIndent()
    {
        $result = "\n\n    This is foo. This is foo. This is\n    foo. This is foo. This is foo. This\n    is foo.\n\n    This is bar.\n\n    bar foo bar foo foo. bar foo bar foo\n    foo. bar foo bar foo foo. bar foo\n    bar foo foo. bar foo bar foo foo.\n    bar foo bar foo foo. bar foo bar foo\n    foo.\n\n";
        $tpl = $this->smarty->createTemplate('eval:{textformat wrap=40 indent=4}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
    }

    public function testIndentWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = "\n\n    This is foo. This is foo. This is\n    foo. This is foo. This is foo. This\n    is foo.\n\n    This is bar.\n\n    bar foo bar foo foo. bar foo bar foo\n    foo. bar foo bar foo foo. bar foo\n    bar foo foo. bar foo bar foo foo.\n    bar foo bar foo foo. bar foo bar foo\n    foo.\n\n";
        $tpl = $this->smarty->createTemplate('eval:{textformat wrap=40 indent=4}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
        Smarty::$_MBSTRING = true;
    }

    public function testIndentFirst()
    {
        $result = "\n\n        This is foo. This is foo. This\n    is foo. This is foo. This is foo.\n    This is foo.\n\n        This is bar.\n\n        bar foo bar foo foo. bar foo bar\n    foo foo. bar foo bar foo foo. bar\n    foo bar foo foo. bar foo bar foo\n    foo. bar foo bar foo foo. bar foo\n    bar foo foo.\n\n";
        $tpl = $this->smarty->createTemplate('eval:{textformat wrap=40 indent=4 indent_first=4}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
    }

    public function testIndentFirstWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = "\n\n        This is foo. This is foo. This\n    is foo. This is foo. This is foo.\n    This is foo.\n\n        This is bar.\n\n        bar foo bar foo foo. bar foo bar\n    foo foo. bar foo bar foo foo. bar\n    foo bar foo foo. bar foo bar foo\n    foo. bar foo bar foo foo. bar foo\n    bar foo foo.\n\n";
        $tpl = $this->smarty->createTemplate('eval:{textformat wrap=40 indent=4 indent_first=4}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
        Smarty::$_MBSTRING = true;
    }

    public function testIndentchar()
    {
        $result = "\n\n####This is foo. This is foo. This is\n####foo. This is foo. This is foo. This\n####is foo.\n\n####This is bar.\n\n####bar foo bar foo foo. bar foo bar foo\n####foo. bar foo bar foo foo. bar foo\n####bar foo foo. bar foo bar foo foo.\n####bar foo bar foo foo. bar foo bar foo\n####foo.\n\n";
        $tpl = $this->smarty->createTemplate('eval:{textformat wrap=40 indent=4 indent_char="#"}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
    }

    public function testIndentcharWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = "\n\n####This is foo. This is foo. This is\n####foo. This is foo. This is foo. This\n####is foo.\n\n####This is bar.\n\n####bar foo bar foo foo. bar foo bar foo\n####foo. bar foo bar foo foo. bar foo\n####bar foo foo. bar foo bar foo foo.\n####bar foo bar foo foo. bar foo bar foo\n####foo.\n\n";
        $tpl = $this->smarty->createTemplate('eval:{textformat wrap=40 indent=4 indent_char="#"}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
        Smarty::$_MBSTRING = true;
    }

    public function testIndentcharFirst()
    {
        $result = "\n\n########This is foo. This is foo. This\n####is foo. This is foo. This is foo.\n####This is foo.\n\n########This is bar.\n\n########bar foo bar foo foo. bar foo bar\n####foo foo. bar foo bar foo foo. bar\n####foo bar foo foo. bar foo bar foo\n####foo. bar foo bar foo foo. bar foo\n####bar foo foo.\n\n";
        $tpl = $this->smarty->createTemplate('eval:{textformat wrap=40 indent=4 indent_first=4 indent_char="#"}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
    }

    public function testIndentcharFirstWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = "\n\n########This is foo. This is foo. This\n####is foo. This is foo. This is foo.\n####This is foo.\n\n########This is bar.\n\n########bar foo bar foo foo. bar foo bar\n####foo foo. bar foo bar foo foo. bar\n####foo bar foo foo. bar foo bar foo\n####foo. bar foo bar foo foo. bar foo\n####bar foo foo.\n\n";
        $tpl = $this->smarty->createTemplate('eval:{textformat wrap=40 indent=4 indent_first=4 indent_char="#"}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
        Smarty::$_MBSTRING = true;
    }

    public function testWrapchar()
    {
        $result = "##    This is foo. This is foo. This is#foo. This is foo. This is foo. This#is foo.##    This is bar.##    bar foo bar foo foo. bar foo bar foo#foo. bar foo bar foo foo. bar foo#bar foo foo. bar foo bar foo foo.#bar foo bar foo foo. bar foo bar foo#foo.##";
        $tpl = $this->smarty->createTemplate('eval:{textformat wrap=40 indent=4 wrap_char="#"}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
    }

    public function testWrapcharWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = "##    This is foo. This is foo. This is#foo. This is foo. This is foo. This#is foo.##    This is bar.##    bar foo bar foo foo. bar foo bar foo#foo. bar foo bar foo foo. bar foo#bar foo foo. bar foo bar foo foo.#bar foo bar foo foo. bar foo bar foo#foo.##";
        $tpl = $this->smarty->createTemplate('eval:{textformat wrap=40 indent=4 wrap_char="#"}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
        Smarty::$_MBSTRING = true;
    }

    public function testStyleEmail()
    {
        $result = "\n\nThis is foo. This is foo. This is foo. This is foo. This is foo. This is\nfoo.\n\nThis is bar.\n\nbar foo bar foo foo. bar foo bar foo foo. bar foo bar foo foo. bar foo\nbar foo foo. bar foo bar foo foo. bar foo bar foo foo. bar foo bar foo\nfoo.\n\n";
        $tpl = $this->smarty->createTemplate('eval:{textformat style="email"}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
    }

    public function testStyleEmailWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $result = "\n\nThis is foo. This is foo. This is foo. This is foo. This is foo. This is\nfoo.\n\nThis is bar.\n\nbar foo bar foo foo. bar foo bar foo foo. bar foo bar foo foo. bar foo\nbar foo foo. bar foo bar foo foo. bar foo bar foo foo. bar foo bar foo\nfoo.\n\n";
        $tpl = $this->smarty->createTemplate('eval:{textformat style="email"}' . $this->string . '{/textformat}');
        $this->assertEquals($this->normalizeString($result), $this->normalizeString($this->smarty->fetch($tpl)));
        Smarty::$_MBSTRING = true;
    }
}
