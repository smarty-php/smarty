<?php
/**
 * Smarty PHPUnit tests demo resource plugin extendsall
 *

 * @author  Uwe Tews
 */

require_once __DIR__ . '/../../../__shared/resources/resource.extendsall.php';

/**
 * class for demo resource plugin extendsall tests
 *
 * 
 * @preserveGlobalState    disabled
 * 
 */
class ResourceExtendsAllPluginTest extends PHPUnit_Smarty
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
     * test  extendsall
     */
    public function testResourcePluginExtendsall()
    {
		$this->smarty->registerResource('extendsall', new My_Resource_Extendsall());
        $this->smarty->setTemplateDir(array('root' => './templates', './templates_2', './templates_3',
                                            './templates_4',));

        $expected = "templatestemplates_3templatestemplates_4";
        $this->assertEquals($expected, $this->smarty->fetch('extendsall:extendsall.tpl'));
    }

    /**
     * test  extendsall
     * changed tepmplate_setting
     */

    public function testResourcePluginExtendsall2()
    {
	    $this->smarty->registerResource('extendsall', new My_Resource_Extendsall());
		$this->smarty->setMergeCompiledIncludes(true);
        $this->smarty->setTemplateDir(array('./templates_3', './templates_4',));

        $expected = "templates_3templates_3templates_3templates_4";
        $this->assertEquals($expected, $this->smarty->fetch('extendsall:extendsall.tpl'));
    }

    public function testResourcePluginExtendsallOne()
    {
	    $this->smarty->registerResource('extendsall', new My_Resource_Extendsall());
        $this->smarty->setTemplateDir(array('root' => './templates', './templates_2', './templates_3',
                                            './templates_4',));

        $expected = "templatestemplates";
        $this->assertEquals($expected, $this->smarty->fetch('extendsall:extendsall2.tpl'));
    }

    /**
     * test  extendsall special application
     */
    public function testResourcePluginExtendsallSpecialApplication()
    {
	    $this->smarty->registerResource('extendsall', new My_Resource_Extendsall());
        $this->smarty->setTemplateDir(array('./templates_2', './templates_1',));
        $this->smarty->setDefaultResourceType('extendsall');
        $this->assertEquals('<p>data1 from templates1</p><p>data1 from templates2</p><p>data2 from templates1</p>',
                            $this->smarty->fetch('template.tpl'));
    }

    /**
     * test  extendsall special application
     */
    public function testResourcePluginExtendsallSpecialApplication2()
    {
	    $this->smarty->registerResource('extendsall', new My_Resource_Extendsall());
        $this->smarty->setTemplateDir(array('./templates_3', './templates_2', './templates_1',));
        $this->smarty->setDefaultResourceType('extendsall');
        $this->assertEquals('<p>data1 from templates1</p><p>data1 from templates3</p><p>data1 from templates2</p><p>data2 from templates1</p>',
                            $this->smarty->fetch('template.tpl'));
    }

    /**
     * test  extendsall special application
     */
    public function testResourcePluginExtendsallSpecialApplication3()
    {
        $this->smarty->setMergeCompiledIncludes(true);
	    $this->smarty->registerResource('extendsall', new My_Resource_Extendsall());
        $this->smarty->setTemplateDir(array('./templates_2', './templates_1',));
        $this->smarty->setDefaultResourceType('extendsall');
        $this->assertEquals('<p>data1 from templates1</p><p>data1 from templates2</p><p>data2 from templates1</p>',
                            $this->smarty->fetch('template.tpl'));
    }

    /**
     * test  extendsall special application
     */
    public function testResourcePluginExtendsallSpecialApplication4()
    {
        $this->smarty->setMergeCompiledIncludes(true);
	    $this->smarty->registerResource('extendsall', new My_Resource_Extendsall());
        $this->smarty->setTemplateDir(array('./templates_2', './templates_1',));
        $this->smarty->setDefaultResourceType('extendsall');
        $this->assertEquals('<p>data1 from templates1</p><p>data1 from templates2</p><p>data2 from templates1</p>',
                            $this->smarty->fetch('template.tpl'));
    }

    /**
     * test  extendsall special application
     */
    public function testResourcePluginExtendsallSpecialApplication5()
    {
        $this->smarty->setMergeCompiledIncludes(true);
	    $this->smarty->registerResource('extendsall', new My_Resource_Extendsall());
        $this->smarty->setTemplateDir(array('./templates_3', './templates_2', './templates_1',));
        $this->smarty->setDefaultResourceType('extendsall');
        $this->assertEquals('<p>data1 from templates1</p><p>data1 from templates3</p><p>data1 from templates2</p><p>data2 from templates1</p>',
                            $this->smarty->fetch('template.tpl'));
    }

    /**
     * test  extendsall special application
     */
    public function testResourcePluginExtendsallSpecialApplication6()
    {
        $this->smarty->setMergeCompiledIncludes(true);
	    $this->smarty->registerResource('extendsall', new My_Resource_Extendsall());
        $this->smarty->setTemplateDir(array('./templates_3', './templates_2', './templates_1',));
        $this->smarty->setDefaultResourceType('extendsall');
        $this->assertEquals('<p>data1 from templates1</p><p>data1 from templates3</p><p>data1 from templates2</p><p>data2 from templates1</p>',
                            $this->smarty->fetch('template.tpl'));
    }
}
