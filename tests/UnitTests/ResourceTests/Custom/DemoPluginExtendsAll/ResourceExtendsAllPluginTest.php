<?php
/**
 * Smarty PHPUnit tests demo resource plugin extendsall
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for demo resource plugin extendsall tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class ResourceExtendsAllPluginTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test  extendsall
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testResourcePluginExtendsall()
    {
        $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
        $this->smarty->setTemplateDir(array('root' => './templates', './templates_2', './templates_3',
                                            './templates_4',));

        $expected = "templatestemplates_3templatestemplates_4";
        $this->assertEquals($expected, $this->smarty->fetch('extendsall:extendsall.tpl'));
    }

    /**
     * test  extendsall
     * changed tepmplate_setting
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */

    public function testResourcePluginExtendsall2()
    {
        $this->smarty->setMergeCompiledIncludes(true);
        $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
        $this->smarty->setTemplateDir(array('./templates_3', './templates_4',));

        $expected = "templates_3templates_3templates_3templates_4";
        $this->assertEquals($expected, $this->smarty->fetch('extendsall:extendsall.tpl'));
    }

    public function testResourcePluginExtendsallOne()
    {
        $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
        $this->smarty->setTemplateDir(array('root' => './templates', './templates_2', './templates_3',
                                            './templates_4',));

        $expected = "templatestemplates";
        $this->assertEquals($expected, $this->smarty->fetch('extendsall:extendsall2.tpl'));
    }

    /**
     * test  extendsall special application
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testResourcePluginExtendsallSpecialApplication()
    {
        $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
        $this->smarty->setTemplateDir(array('./templates_2', './templates_1',));
        $this->smarty->setDefaultResourceType('extendsall');
        $this->assertEquals('<p>data1 from templates1</p><p>data1 from templates2</p><p>data2 from templates1</p>',
                            $this->smarty->fetch('template.tpl'));
    }

    /**
     * test  extendsall special application
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testResourcePluginExtendsallSpecialApplication2()
    {
        $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
        $this->smarty->setTemplateDir(array('./templates_3', './templates_2', './templates_1',));
        $this->smarty->setDefaultResourceType('extendsall');
        $this->assertEquals('<p>data1 from templates1</p><p>data1 from templates3</p><p>data1 from templates2</p><p>data2 from templates1</p>',
                            $this->smarty->fetch('template.tpl'));
    }

    /**
     * test  extendsall special application
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testResourcePluginExtendsallSpecialApplication3()
    {
        $this->smarty->setMergeCompiledIncludes(true);
        $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
        $this->smarty->setTemplateDir(array('./templates_2', './templates_1',));
        $this->smarty->setDefaultResourceType('extendsall');
        $this->assertEquals('<p>data1 from templates1</p><p>data1 from templates2</p><p>data2 from templates1</p>',
                            $this->smarty->fetch('template.tpl'));
    }

    /**
     * test  extendsall special application
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testResourcePluginExtendsallSpecialApplication4()
    {
        $this->smarty->setMergeCompiledIncludes(true);
        $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
        $this->smarty->setTemplateDir(array('./templates_2', './templates_1',));
        $this->smarty->setDefaultResourceType('extendsall');
        $this->assertEquals('<p>data1 from templates1</p><p>data1 from templates2</p><p>data2 from templates1</p>',
                            $this->smarty->fetch('template.tpl'));
    }

    /**
     * test  extendsall special application
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testResourcePluginExtendsallSpecialApplication5()
    {
        $this->smarty->setMergeCompiledIncludes(true);
        $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
        $this->smarty->setTemplateDir(array('./templates_3', './templates_2', './templates_1',));
        $this->smarty->setDefaultResourceType('extendsall');
        $this->assertEquals('<p>data1 from templates1</p><p>data1 from templates3</p><p>data1 from templates2</p><p>data2 from templates1</p>',
                            $this->smarty->fetch('template.tpl'));
    }

    /**
     * test  extendsall special application
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testResourcePluginExtendsallSpecialApplication6()
    {
        $this->smarty->setMergeCompiledIncludes(true);
        $this->smarty->addPluginsDir(SMARTY_DIR . "../demo/plugins/");
        $this->smarty->setTemplateDir(array('./templates_3', './templates_2', './templates_1',));
        $this->smarty->setDefaultResourceType('extendsall');
        $this->assertEquals('<p>data1 from templates1</p><p>data1 from templates3</p><p>data1 from templates2</p><p>data2 from templates1</p>',
                            $this->smarty->fetch('template.tpl'));
    }
}
