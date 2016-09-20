<?php
/**
 * Smarty PHPunit tests for deleting compiled templates
 *
 * @package PHPunit
 * @author  Uwe Tews
 * @author  Rodney Rehm
 */
include_once dirname(__FILE__) . '/ClearCompiledTest.php';

/**
 * class for delete compiled template tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class ClearCompiledBCTest extends ClearCompiledTest
{
    public $loadSmartyBC = true;
    public $loadSmarty = false;

    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smartyBC->addTemplateDir('./templates_2/');
        $this->methodName = 'clear_compiled_tpl';
    }
}
