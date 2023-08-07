<?php
/**
 * Smarty PHPunit tests of modifier
 *

 * @author  Rodney Rehm
 */

/**
 * class for modifier tests
 *
 *
 *
 *
 */
class PluginBlockTextformatTest extends PHPUnit_Smarty
{
    protected $string = "\n\nThis is foo.\nThis is foo.\nThis is foo.\nThis is foo.\nThis is foo.\nThis is foo.\n\nThis is bar.\n\nbar foo bar foo     foo.\nbar foo bar foo     foo.\nbar foo bar foo     foo.\nbar foo bar foo     foo.\nbar foo bar foo     foo.\nbar foo bar foo     foo.\nbar foo bar foo     foo.\n\n";

    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

	public function testInit()
	{
		$this->cleanDirs();
	}

    public function testDefault()
    {
        $result = "This is foo. This is foo. This is foo.\nThis is foo. This is foo. This is foo.\n\nThis is bar.\n\nbar foo bar foo foo. bar foo bar foo\nfoo. bar foo bar foo foo. bar foo bar\nfoo foo. bar foo bar foo foo. bar foo\nbar foo foo. bar foo bar foo foo.\n\n";
        $tpl = $this->smarty->createTemplate('string:{textformat wrap=40}' . $this->string . '{/textformat}');
        $this->assertEquals($result, $this->smarty->fetch($tpl));
    }

    public function testIndent()
    {
        $result = "    This is foo. This is foo. This is\n    foo. This is foo. This is foo. This\n    is foo.\n\n    This is bar.\n\n    bar foo bar foo foo. bar foo bar foo\n    foo. bar foo bar foo foo. bar foo\n    bar foo foo. bar foo bar foo foo.\n    bar foo bar foo foo. bar foo bar foo\n    foo.\n\n";
        $tpl = $this->smarty->createTemplate('string:{textformat wrap=40 indent=4}' . $this->string . '{/textformat}');
        $this->assertEquals($result, $this->smarty->fetch($tpl));
    }

    public function testIndentFirst()
    {
        $result = "        This is foo. This is foo. This\n    is foo. This is foo. This is foo.\n    This is foo.\n\n        This is bar.\n\n        bar foo bar foo foo. bar foo bar\n    foo foo. bar foo bar foo foo. bar\n    foo bar foo foo. bar foo bar foo\n    foo. bar foo bar foo foo. bar foo\n    bar foo foo.\n\n";
        $tpl = $this->smarty->createTemplate('string:{textformat wrap=40 indent=4 indent_first=4}' . $this->string . '{/textformat}');
        $this->assertEquals($result, $this->smarty->fetch($tpl));
    }

    public function testIndentchar()
    {
        $result = "####This is foo. This is foo. This is\n####foo. This is foo. This is foo. This\n####is foo.\n\n####This is bar.\n\n####bar foo bar foo foo. bar foo bar foo\n####foo. bar foo bar foo foo. bar foo\n####bar foo foo. bar foo bar foo foo.\n####bar foo bar foo foo. bar foo bar foo\n####foo.\n\n";
        $tpl = $this->smarty->createTemplate('string:{textformat wrap=40 indent=4 indent_char="#"}' . $this->string . '{/textformat}');
        $this->assertEquals($result, $this->smarty->fetch($tpl));
    }

    public function testIndentcharFirst()
    {
        $result = "########This is foo. This is foo. This\n####is foo. This is foo. This is foo.\n####This is foo.\n\n########This is bar.\n\n########bar foo bar foo foo. bar foo bar\n####foo foo. bar foo bar foo foo. bar\n####foo bar foo foo. bar foo bar foo\n####foo. bar foo bar foo foo. bar foo\n####bar foo foo.\n\n";
        $tpl = $this->smarty->createTemplate('string:{textformat wrap=40 indent=4 indent_first=4 indent_char="#"}' . $this->string . '{/textformat}');
        $this->assertEquals($result, $this->smarty->fetch($tpl));
    }

    public function testWrapchar()
    {
        $result = "    This is foo. This is foo. This is#foo. This is foo. This is foo. This#is foo.##    This is bar.##    bar foo bar foo foo. bar foo bar foo#foo. bar foo bar foo foo. bar foo#bar foo foo. bar foo bar foo foo.#bar foo bar foo foo. bar foo bar foo#foo.##";
        $tpl = $this->smarty->createTemplate('string:{textformat wrap=40 indent=4 wrap_char="#"}' . $this->string . '{/textformat}');
        $this->assertEquals($result, $this->smarty->fetch($tpl));
    }

    public function testStyleEmail()
    {
        $result = "This is foo. This is foo. This is foo. This is foo. This is foo. This is\nfoo.\n\nThis is bar.\n\nbar foo bar foo foo. bar foo bar foo foo. bar foo bar foo foo. bar foo\nbar foo foo. bar foo bar foo foo. bar foo bar foo foo. bar foo bar foo\nfoo.\n\n";
        $tpl = $this->smarty->createTemplate('string:{textformat style="email"}' . $this->string . '{/textformat}');
        $this->assertEquals($result, $this->smarty->fetch($tpl));
    }

}
