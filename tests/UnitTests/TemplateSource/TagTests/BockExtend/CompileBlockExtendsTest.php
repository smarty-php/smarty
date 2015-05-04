<?php
/**
 * Smarty PHPunit tests for Block Extends
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for block extends compiler tests
 *
 * @backupStaticAttributes enabled
 */
class CompileBlockExtendsTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test block default outout
     */
    public function testBlockDefault_000_1()
    {
        $result = $this->smarty->fetch('string:{block name=test}-- block default --{/block}');
        $this->assertEquals('-- block default --', $result);
    }

    public function testBlockDefault_000_2()
    {
        $this->smarty->assign('foo', 'another');
        $result = $this->smarty->fetch('string:{block name=test}-- {$foo} block default --{/block}');
        $this->assertEquals('-- another block default --', $result);
    }

    /**
     * test just call of  parent template, no blocks predefined
     */
    public function testCompileBlockParent_001()
    {
        $result = $this->smarty->fetch('001_parent.tpl');
        $this->assertContains('Default Title', $result);
    }

    /**
     * test  child/parent template chain
     */
    public function testCompileBlockChild_002()
    {
        $result = $this->smarty->fetch('002_child.tpl');
        $this->assertContains('Page Title', $result);
    }

    /**
     * test  child/parent template chain with prepend
     */
    public function testCompileBlockChildPrepend_003()
    {
        $result = $this->smarty->fetch('003_child_prepend.tpl');
        $this->assertContains("prepend - Default Title", $result);
    }

    /**
     * test  child/parent template chain with apppend
     */
    public function testCompileBlockChildAppend_004()
    {
        $result = $this->smarty->fetch('004_child_append.tpl');
        $this->assertContains("Default Title - append", $result);
    }

    /**
     * test  child/parent template chain with apppend and shorttags
     */
    public function testCompileBlockChildAppendShortag_005()
    {
        $result = $this->smarty->fetch('005_child_append_shorttag.tpl');
        $this->assertContains("Default Title - append", $result);
    }

    /**
     * test  child/parent template chain with {$this->smarty.block.child)
     */
    public function testCompileBlockChildSmartyChild_006()
    {
        $result = $this->smarty->fetch('006_child_smartychild.tpl');
        $this->assertContains('here is >child text< included', $result);
    }

    /**
     * test  child/parent template chain with {$this->smarty.block.parent)
     */
    public function testCompileBlockChildSmartyParent_007()
    {
        $result = $this->smarty->fetch('007_child_smartyparent.tpl');
        $this->assertContains('parent block Default Title is here', $result);
    }

    /**
     * test  child/parent template chain loading plugin
     */
    public function testCompileBlockChildPlugin_008()
    {
        $result = $this->smarty->fetch('008_child_plugin.tpl');
        $this->assertContains('escaped &lt;text&gt;', $result);
    }

    /**
     * test parent template with nested blocks
     */
    public function testCompileBlockParentNested_009()
    {
        $result = $this->smarty->fetch('009_parent_nested.tpl');
        $this->assertContains('Title with -default- here', $result);
    }

    /**
     * test  child/parent template chain with nested block
     */
    public function testCompileBlockChildNested_010()
    {
        $result = $this->smarty->fetch('010_child_nested.tpl');
        $this->assertContains('Title with -content from child- here', $result);
    }

    /**
     * test  child/parent template chain with nested block and include
     */
    public function testCompileBlockChildNestedInclude_011()
    {
        $result = $this->smarty->fetch('011_grandchild_nested_include.tpl');
        $this->assertContains('hello world', $result);
    }

    /**
     * test  grandchild/child/parent template chain
     */
    public function testCompileBlockGrandChild_012()
    {
        $result = $this->smarty->fetch('012_grandchild.tpl');
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent template chain prepend
     */
    public function testCompileBlockGrandChildPrepend_013()
    {
        $result = $this->smarty->fetch('013_grandchild_prepend.tpl');
        $this->assertContains('grandchild prepend - Page Title', $result);
    }

    /**
     * test  grandchild/child/parent template chain with {$this->smarty.block.child}
     */
    public function testCompileBlockGrandChildSmartyChild_014()
    {
        $result = $this->smarty->fetch('014_grandchild_smartychild.tpl');
        $this->assertContains('child title with - grandchild content - here', $result);
    }

    /**
     * test  grandchild/child/parent template chain append
     */
    public function testCompileBlockGrandChildAppend_015()
    {
        $result = $this->smarty->fetch('015_grandchild_append.tpl');
        $this->assertContains('Page Title - grandchild append', $result);
    }

    /**
     * test  grandchild/child/parent template chain with nested block
     */
    public function testCompileBlockGrandChildNested_016()
    {
        $result = $this->smarty->fetch('016_grandchild_nested.tpl');
        $this->assertContains('child title with -grandchild content- here', $result);
    }

    /**
     * test  grandchild/child/parent template chain with nested {$this->smarty.block.child}
     */
    public function testCompileBlockGrandChildNested_017()
    {
        $result = $this->smarty->fetch('017_grandchild_nested.tpl');
        $this->assertContains('child pre -grandchild content- child post', $result);
    }

    /**
     * test  nested child block with hide
     */
    public function testCompileBlockChildNestedHide_018()
    {
        $result = $this->smarty->fetch('018_child_nested_hide.tpl');
        $this->assertContains('nested block', $result);
        $this->assertNotContains('should be hidden', $result);
    }

    /**
     * test  nested child block with hide and auto_literal = false
     */
    public function testCompileBlockChildNestedHideAutoLiteralFalse_019()
    {
        $this->smarty->setAutoLiteral(false);
        $result = $this->smarty->fetch('019_child_nested_hide_autoliteral.tpl');
        $this->assertContains('nested block', $result);
        $this->assertNotContains('should be hidden', $result);
    }

    /**
     * test  child/parent template chain starting in subtempates
     */
    public function testCompileBlockStartSubTemplates_020()
    {
        $result = $this->smarty->fetch('020_include_root.tpl');
        $this->assertContains('page 1', $result);
        $this->assertContains('page 2', $result);
        $this->assertContains('page 3', $result);
        $this->assertContains('block 1', $result);
        $this->assertContains('block 2', $result);
        $this->assertContains('block 3', $result);
    }

    /**
     * test  grandchild/child/parent dependency test1
     */
    public function testCompileBlockGrandChildMustCompile_021_1()
    {
        $this->cleanDirs();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
        $this->smarty->template_objects = null;
        $tpl2 = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent dependency test2
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_2()
    {
        sleep(2);
        touch($this->smarty->getTemplateDir(0) . '021_grandchild.tpl');
        clearstatcache();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
        $this->smarty->template_objects = null;
        $tpl2 = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent dependency test3
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_3()
    {
        sleep(2);
        touch($this->smarty->getTemplateDir(0) . '021_child.tpl');
        clearstatcache();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
        $this->smarty->template_objects = null;
        $tpl2 = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent dependency test4
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_4()
    {
        sleep(2);
        touch($this->smarty->getTemplateDir(0) . '021_parent.tpl');
        clearstatcache();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
        $this->smarty->template_objects = null;
        $tpl2 = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test dirt in child templates
     */
    public function testDirt_022()
    {
        $result = $this->smarty->fetch('022_child.tpl');
        $this->assertEquals('Page Title', $result);
    }

    /**
     * test {$this->smarty.block.child} for not existing child {block]
     */
    public function testNotExistingChildBlock_024()
    {
        $result = $this->smarty->fetch('024_parent.tpl');
        $this->assertContains('no >< child', $result);
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Syntax error in template "./templates/025_parent.tpl"
     * @expectedExceptionMessage tag {$smarty.block.child} used outside {block} tags
     * test {$this->smarty.block.child} outside {block]
     */
    public function testSmartyBlockChildOutsideBlock_025()
    {
         $this->smarty->fetch('025_parent.tpl');
     }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Syntax error in template "./templates/026_parent.tpl"
     * @expectedExceptionMessage tag {$smarty.block.parent} used outside {block} tags
     * test {$this->smarty.block.parent} outside {block]
     */
    public function testSmartyBlockParentOutsideBlock_026()
    {
        $this->smarty->fetch('026_parent.tpl');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Syntax error in template "./templates/027_parent.tpl"
     * @expectedExceptionMessage illegal {$smarty.block.parent} in parent template
     * test {$this->smarty.block.parent} in parent template
     */
    public function testSmartyBlockParentInParent_027()
    {
        $result = $this->smarty->fetch('027_parent.tpl');
    }
}
