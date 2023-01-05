<?php
/**
 * Smarty PHPunit tests compilation of {section} tag
 *

 * @author  Uwe Tews
 */

/**
 * class for {section} tag tests
 *
 * 
 * 
 * 
 */
class CompileSectionTest extends PHPUnit_Smarty
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
     * test {section} tag
     */
    public function testSection_001()
    {
        $this->smarty->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("0123456789", $this->smarty->fetch('001_section.tpl'));
    }

    public function testSection_002()
    {
        $this->smarty->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("0123456789", $this->smarty->fetch('002_section.tpl'));
    }

    public function testSection_003()
    {
        $this->assertEquals("else", $this->smarty->fetch('003_section.tpl'));
    }

    public function testSection_004()
    {
        $this->smarty->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("0123456789", $this->smarty->fetch('004_section.tpl'));
    }

    public function testSection_006()
    {
        $this->smarty->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("0123456789total10", $this->smarty->fetch('006_section.tpl'));
    }

    public function testSection_007()
    {
        $this->smarty->assign('foo', array(0,1,2,3,4,5,6,7,8,9));
        $this->assertEquals("011223344556677889910", $this->smarty->fetch('007_section.tpl'));
    }
    /**
     * Test spacings
     *
     * 
     * @dataProvider        dataTestSpacing
     * 
     */
    public function testSpacing($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', array(1,2));
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            $file);
    }

    /*
      * Data provider für testSpacing
      */
    public function dataTestSpacing()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(array("A{section name=bar loop=\$foo}{\$foo[bar]}{/section}C", "A12C", 'T1', $i++),
                     array("A{section name=bar loop=\$foo}\n{\$foo[bar]}{/section}C", "A12C", 'T2', $i++),
                     array("A{section name=bar loop=\$foo}{\$foo[bar]}\n{/section}C", "A1\n2\nC", 'T3', $i++),
                     array("A{section name=bar loop=\$foo}\n{\$foo[bar]}\n{/section}C", "A1\n2\nC", 'T4', $i++),
                     array("A\n{section name=bar loop=\$foo}{\$foo[bar]}{/section}C", "A\n12C", 'T5', $i++),
                     array("A{section name=bar loop=\$foo}{\$foo[bar]}{/section}\nC", "A12C", 'T6', $i++),
                     array("A{section name=bar loop=\$foo}{\$foo[bar]}{sectionelse}D{/section}C", "A12C", 'T7', $i++),
                     array("A{section name=bar loop=\$foo}{\$foo[bar]}\n{sectionelse}D{/section}C", "A1\n2\nC", 'T8', $i++),
                     array("{section loop=\$foo name='bar'}{\$foo[bar]}{/section}A{\$smarty.section.bar.total}C", "12A2C", 'T9', $i++),
                     array("{section loop=\$foo name='bar'}{\$foo[bar]}{/section}A\n{\$smarty.section.bar.total}C", "12A\n2C", 'T10', $i++),
                     array("{section loop=\$foo name='bar'}{\$foo[bar]}{/section}A{\$smarty.section.bar.total}\nC", "12A2\nC", 'T11', $i++),
        );
    }
    /**
     * Test spacings
     *
     * 
     * @dataProvider        dataTestElseSpacing
     * 
     */
    public function testElseSpacing($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_Else_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', array());
        $this->smarty->assign('bar', 'bar');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            $file);
    }

    /*
      * Data provider für testSpacing
      */
    public function dataTestElseSpacing()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(
            array("{section name=bar loop=\$foo}{\$foo[bar]}{sectionelse}A{\$bar}B{/section}", "AbarB", 'T1', $i++),
            array("{section name=bar loop=\$foo}{\$foo[bar]}{sectionelse}\nA{\$bar}B{/section}", "AbarB", 'T2', $i++),
            array("{section name=bar loop=\$foo}{\$foo[bar]}{sectionelse}A{\$bar}\nB{/section}", "Abar\nB", 'T3', $i++),
            array("{section name=bar loop=\$foo}{\$foo[bar]}{sectionelse}\nA{\$bar}\nB{/section}", "Abar\nB", 'T4', $i++),
            array("{section name=bar loop=\$foo}{\$foo[bar]}{sectionelse}{\$bar}\nB{/section}", "bar\nB", 'T5', $i++),
            array("{section name=bar loop=\$foo}{\$foo[bar]}{sectionelse}{\$bar}{/section}", "bar", 'T6', $i++),
        );
    }

}
