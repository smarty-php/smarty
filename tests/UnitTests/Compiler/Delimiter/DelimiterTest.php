<?php
/**
 * Smarty PHPunit tests of delimiter
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for delimiter tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class DelimiterTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
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
        $this->smarty->left_delimiter = '<{';
        $this->smarty->right_delimiter = '}>';
        $tpl = $this->smarty->createTemplate('eval:start <{* comment *}>hello <{if true}><{"world"}><{/if}> end');
        $this->assertEquals("start hello world end", $this->smarty->fetch($tpl));
    }
    /**
     * test <{ }> delimiter
     */
    public function testDelimiter10()
    {
        $this->smarty->left_delimiter = '<';
        $this->smarty->right_delimiter = '>';
        $tpl = $this->smarty->createTemplate('eval:start <* comment *>hello <if 1 < 2><"world"></if> end');
        $this->assertEquals("start hello world end", $this->smarty->fetch($tpl));
    }

    /**
     * test <-{ }-> delimiter
     */
    public function testDelimiter2()
    {
        $this->smarty->left_delimiter = '<-{';
        $this->smarty->right_delimiter = '}->';
        $tpl = $this->smarty->createTemplate('eval:<-<-{* comment *}-><-{if true}-><-{"hello world"}-><-{/if}->->');
        $this->assertEquals("<-hello world->", $this->smarty->fetch($tpl));
    }

    /**
     * test <--{ }--> delimiter
     */
    public function testDelimiter3()
    {
        $this->smarty->left_delimiter = '<--{';
        $this->smarty->right_delimiter = '}-->';
        $tpl = $this->smarty->createTemplate('eval:<--{* comment *}--><--{if true}--><--{"hello world"}--><--{/if}-->');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test {{ }} delimiter
     */
    public function testDelimiter4()
    {
        $this->smarty->left_delimiter = '{{';
        $this->smarty->right_delimiter = '}}';
        $tpl = $this->smarty->createTemplate('eval:{{* comment *}}{{if true}}{{"hello world"}}{{/if}}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }

    /**
     * test {= =} delimiter for conficts with option flags
     */
    public function testDelimiter5()
    {
        $this->smarty->left_delimiter = '{=';
        $this->smarty->right_delimiter = '=}';
        $tpl = $this->smarty->createTemplate('eval:{=assign var=foo value="hello world" nocache=}{=$foo=}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }
    /**
     * test {= =} delimiter for conficts with option flags
     */
    public function testDelimiterIssue450()
    {
        $this->smarty->left_delimiter = '{^';
        $this->smarty->right_delimiter = '^}';
        $tpl = $this->smarty->createTemplate('eval:{^assign var=foo value="hello world" nocache^}{^$foo^}');
        $this->assertEquals("hello world", $this->smarty->fetch($tpl));
    }
}
