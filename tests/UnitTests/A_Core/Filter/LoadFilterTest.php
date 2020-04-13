<?php
/**
 * Smarty PHPunit tests loadFilter method
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for loadFilter method tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class LoadFilterTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    /**
     * test loadFilter method
     */
    public function testLoadFilter()
    {
        $this->smarty->loadFilter('output', 'trimwhitespace');
        $this->assertTrue(is_callable($this->smarty->registered_filters['output']['smarty_outputfilter_trimwhitespace']));
    }
}
