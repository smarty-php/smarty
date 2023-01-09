<?php
/**
 * Smarty PHPunit tests of delimiter
 *

 * @author  Uwe Tews
 */

/**
 * class for delimiter tests
 *
 * 
 * 
 *
 */
class DelimiterTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test <{ }> delimiter
     */
    public function testDelimiter1()
    {
        $this->smarty->setLeftDelimiter('<{');
        $this->smarty->setRightDelimiter('}>');
        $tpl = $this->smarty->createTemplate('eval:start <{* comment *}>hello <{if true}><{"world"}><{/if}> end');
        $this->assertEquals("start hello world end", $this->smarty->fetch($tpl));
    }
    /**
     * test <{ }> delimiter
     */
    public function testDelimiter10()
    {
        $this->smarty->setLeftDelimiter('<');
        $this->smarty->setRightDelimiter('>');
        $tpl = $this->smarty->createTemplate('eval:start <* comment *>hello <if 1 < 2><"world"></if> end');
        $this->assertEquals("start hello world end", $this->smarty->fetch($tpl));
    }

    /**
     * test <-{ }-> delimiter
     */
    public function testDelimiter2()
    {
        $this->smarty->setLeftDelimiter('<-{');
        $this->smarty->setRightDelimiter('}->');
        $tpl = $this->smarty->createTemplate('eval:<-<-{* comment *}-><-{if true}-><-{"hello world"}-><-{/if}->->');
        $this->assertEquals("<-hello world->", $this->smarty->fetch($tpl));
    }

    /**
     * test <--{ }--> delimiter
     */
    public function testDelimiter3()
    {
        $this->smarty->setLeftDelimiter('<--{');
        $this->smarty->setRightDelimiter('}-->');
        $tpl = $this->smarty->createTemplate('eval:<--{* comment *}--><--{if true}--><--{"hello world"}--><--{/if}-->');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test {{ }} delimiter
     */
    public function testDelimiter4()
    {
        $this->smarty->setLeftDelimiter('{{');
        $this->smarty->setRightDelimiter('}}');
        $tpl = $this->smarty->createTemplate('eval:{{* comment *}}{{if true}}{{"hello world"}}{{/if}}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test {= =} delimiter for conficts with option flags
     */
    public function testDelimiter5()
    {
        $this->smarty->setLeftDelimiter('{=');
        $this->smarty->setRightDelimiter('=}');
        $tpl = $this->smarty->createTemplate('eval:{=assign var=foo value="hello world" nocache=}{=$foo=}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }
    /**
     * test {= =} delimiter for conficts with option flags
     */
    public function testDelimiterIssue450()
    {
        $this->smarty->setLeftDelimiter('{^');
        $this->smarty->setRightDelimiter('^}');
        $tpl = $this->smarty->createTemplate('eval:{^assign var=foo value="hello world" nocache^}{^$foo^}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }
}
