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
class PluginModifierRegexReplaceTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testDefault()
    {
        $tpl = $this->smarty->createTemplate('string:{"Infertility unlikely to\nbe passed on, experts say."|regex_replace:"/[\r\t\n]/":" "}');
        $this->assertEquals("Infertility unlikely to be passed on, experts say.", $this->smarty->fetch($tpl));
    }

    public function testWithNull()
    {
        // the test suite mutes E_DEPRECATED globally, so collect deprecations manually
        $deprecations = [];
        set_error_handler(function ($errno, $errstr) use (&$deprecations) {
            $deprecations[] = $errstr;
            return true;
        }, E_DEPRECATED);

        try {
            $this->smarty->assign('subject', null);
            $tpl = $this->smarty->createTemplate('string:{$subject|regex_replace:"/[\r\t\n]/":" "}');
            $this->assertEquals("", $this->smarty->fetch($tpl));

            // null replacement removes the match
            $this->smarty->assign('replacement', null);
            $tpl = $this->smarty->createTemplate('string:{"a\nb"|regex_replace:"/[\r\t\n]/":$replacement}');
            $this->assertEquals("ab", $this->smarty->fetch($tpl));

            // null limit keeps behaving as 0 (no replacements), as PHP's null-to-int
            // coercion did before the deprecation
            $this->smarty->assign('limit', null);
            $tpl = $this->smarty->createTemplate('string:{"a\nb\nc"|regex_replace:"/[\r\t\n]/":" ":$limit}');
            $this->assertEquals("a\nb\nc", $this->smarty->fetch($tpl));
        } finally {
            restore_error_handler();
        }

        $this->assertEquals([], $deprecations);
    }

    public function testUmlauts()
    {
        $tpl = $this->smarty->createTemplate('string:{"Infertility unlikely tö\näe passed on, experts say."|regex_replace:"/[\r\t\n]/u":" "}');
        $this->assertEquals("Infertility unlikely tö äe passed on, experts say.", $this->smarty->fetch($tpl));

        $tpl = $this->smarty->createTemplate('string:{"Infertility unlikely tä be passed on, experts say."|regex_replace:"/[ä]/ue":"ae"}');
        $this->assertEquals("Infertility unlikely tae be passed on, experts say.", $this->smarty->fetch($tpl));
    }
}
