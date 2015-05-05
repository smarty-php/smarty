<?php
/**
 * Smarty PHPunit tests for Extendsresource
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for extends resource tests
 *
 * @backupStaticAttributes enabled
 */
class ExtendsResourceTest extends PHPUnit_Smarty
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
     * test  child/parent template chain with prepend
     */
    public function testCompileBlockChildPrepend_003()
    {
        $result = $this->smarty->fetch('extends:003_parent.tpl|003_child_prepend.tpl');
        $this->assertContains("prepend - Default Title", $result);
    }
    /**
     * test  child/parent template chain with apppend
     */
    public function testCompileBlockChildAppend_004()
    {
        $this->smarty->merge_compiled_includes = true;
        $result = $this->smarty->fetch('extends:004_parent.tpl|004_child_append.tpl');
        $this->assertContains("Default Title - append", $result);
    }
    /**
     * test  grandchild/child/parent dependency test1
     */
    public function testCompileBlockGrandChildMustCompile_021_1()
    {
        $this->cleanDirs();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
        $this->smarty->template_objects = null;
        $tpl2 = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
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
        $tpl = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
        $this->smarty->template_objects = null;
        $tpl2 = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
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
        $tpl = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
        $this->smarty->template_objects = null;
        $tpl2 = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
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
        $tpl = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
        $this->smarty->template_objects = null;
        $tpl2 = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('Grandchild Page Title', $result);
    }
    /**
     * test  child/parent template chain with prepend
     */
    public function testCompileBlockChildPrepend_0032()
    {
        $this->smarty->caching = true;
        $result = $this->smarty->fetch('extends:003_parent.tpl|003_child_prepend.tpl');
        $this->assertContains("prepend - Default Title", $result);
    }

}

