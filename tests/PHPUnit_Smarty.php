<?php
/*
 * This file is part of the Smarty PHPUnit tests.
 *
 */

/**
 * Smarty Test Case Fixture
 */
class PHPUnit_Smarty extends PHPUnit_Framework_TestCase
{
    /**
     * Smarty object
     *
     * @var SmartyBC
     */
    public $smartyBC = null;

    /**
     * SmartyBC object
     *
     * @var Smarty
     */
    public $smarty = null;

    /**
     * Flag if test is using the Smarty object
     *
     * @var bool
     */
    public $loadSmarty = true;

    /**
     * Flag if test is using the SmartyBC object
     *
     * @var bool
     */
    public $loadSmartyBC = false;

    /**
     * Flag for initialization at first test
     *
     * @var bool
     */
    public static $init = true;

    /**
     * Configuration data from config.xml
     *
     * @var array
     */
    public static $config = null;

    /**
     * Saved current working directory
     *
     * @var null
     */
    public static $cwd = null;

    /**
     * PDO object for Mysql tests
     *
     * @var PDO
     */
    public static $pdo = null;

    /**
     * Default blacklist
     *
     * @var array
     */
    protected $backupStaticAttributesBlacklist = array(
        'PHPUnit_Smarty' => array('config', 'pdo', 'init'),
    );

    /**
     * This method is called before the first test of this test class is run.
     *
     */
    public static function setUpBeforeClass()
    {
        error_reporting(E_ALL | E_STRICT);
        self::$init = true;
        self::$pdo = null;
        if (self::$config == null) {
            $xml = simplexml_load_file(__DIR__ . '/config.xml');
            $json_string = json_encode($xml);
            self::$config = json_decode($json_string, true);
            if (empty(self::$config['mysql']['DB_PASSWD'])) {
                self::$config['mysql']['DB_PASSWD'] = null;
            }
        }
    }

    /**
     * This method is called after the last test of this test class is run.
     *
     */
    public static function tearDownAfterClass()
    {
        self::$pdo = null;
    }

    /**
     * Constructs a test case with the given name.
     *
     * @param string $name
     * @param array  $data
     * @param string $dataName
     */
    public function __construct($name = null, array $data = array(), $dataName = '')
    {
        parent::__construct($name, $data, $dataName);
        $this->backupStaticAttributesBlacklist[get_class($this)] = array('init', 'config', 'pdo');
    }

    /**
     * Setup Smarty instance called for each test
     *
     * @param null $dir working directory
     */
    public function setUpSmarty($dir = null)
    {
        // set up current working directory
        chdir($dir);
        self::$cwd = getcwd();
        // create missing folders for test
        if (self::$init) {
            if (!is_dir($dir . '/templates')) {
                mkdir($dir . '/templates');
            }
            if (!is_dir($dir . '/configs')) {
                mkdir($dir . '/configs');
            }
            if (self::$config['individualFolders'] != 'true') {
                $dir = __DIR__;
            }
            if (!is_dir($dir . '/templates_c')) {
                mkdir($dir . '/templates_c');
                chmod($dir . '/templates_c', 0775);
            }
            if (!is_dir($dir . '/cache')) {
                mkdir($dir . '/cache');
                chmod($dir . '/cache', 0775);
            }
            self::$init = false;
        }
        clearstatcache();
        // instance Smarty class
        if ($this->loadSmarty) {
            $this->smarty = new Smarty;
            if (self::$config['individualFolders'] != 'true') {
                $this->smarty->setCompileDir(__DIR__ . '/templates_c');
                $this->smarty->setCacheDir(__DIR__ . '/cache');
            }
        }
        // instance SmartyBC class
        if ($this->loadSmartyBC) {
            $this->smartyBC = new SmartyBC;
            if (self::$config['individualFolders'] != 'true') {
                $this->smartyBC->setCompileDir(__DIR__ . '/templates_c');
                $this->smartyBC->setCacheDir(__DIR__ . '/cache');
            }
        }
    }

    /**
     * Create Mysql PDO object
     *
     */
    final public function getConnection()
    {
        if (self::$pdo == null) {
            try {
                self::$pdo = new PDO(self::$config['mysql']['DB_DSN'], self::$config['mysql']['DB_USER'], self::$config['mysql']['DB_PASSWD']);
            }
            catch (PDOException $e) {
                throw new SmartyException('Mysql Resource failed: ' . $e->getMessage());
            }
            $timezone = date_default_timezone_get();
            self::$pdo->exec("SET time_zone = '{$timezone}';");
         }
    }


    /**
     * Create table for Mysql resource
     *
     */
    public function initMysqlResource()
    {
        $this->getConnection();
        self::$pdo->exec("DROP TABLE `templates`");
        self::$pdo->exec("CREATE TABLE IF NOT EXISTS `templates` (
 `name` varchar(100) NOT NULL,
 `modified` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
 `source` text,
PRIMARY KEY (`name`)
 ) ENGINE=InnoDB DEFAULT CHARSET=utf8");
    }

    /**
     * Create table for Mysql cache resource
     *
     */
    public function initMysqlCache()
    {
        $this->getConnection();
        self::$pdo->exec("DROP TABLE `output_cache`");
        self::$pdo->exec("CREATE TABLE IF NOT EXISTS `output_cache` (
`id` char(40) NOT NULL COMMENT 'sha1 hash',
`name` varchar(250) NOT NULL,
`cache_id` varchar(250) DEFAULT NULL,
`compile_id` varchar(250) DEFAULT NULL,
`modified` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
`expire` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
`content` mediumblob NOT NULL,
PRIMARY KEY (`id`),
KEY `name` (`name`),
KEY `cache_id` (`cache_id`),
KEY `compile_id` (`compile_id`),
KEY `modified` (`modified`),
KEY `expire` (`expire`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8");
    }

    /**
     * Delete files in templates_c and cache folders
     *
     */
    public function cleanDirs()
    {
        $this->cleanCompileDir();
        $this->cleanCacheDir();
    }

    /**
     * Delete files in templates_c folder
     *
     */
    public function cleanCompileDir()
    {
        if (isset($this->smarty)) {
            $this->cleanDir($this->smarty->getCompileDir());
        } elseif (isset($this->smartyBC)) {
            $this->cleanDir($this->smartyBC->getCompileDir());
        }
    }

    /**
     * Delete files in cache folder
     *
     */
    public function cleanCacheDir()
    {
        if (isset($this->smarty)) {
            $this->cleanDir($this->smarty->getCacheDir());
        } elseif (isset($this->smartyBC)) {
            $this->cleanDir($this->smartyBC->getCacheDir());
        }
    }

    /**
     * Delete files and sub folders
     *
     * @param string $dir
     */
    public function cleanDir($dir)
    {
        $di = new RecursiveDirectoryIterator($dir, FilesystemIterator::SKIP_DOTS);
        $ri = new RecursiveIteratorIterator($di, RecursiveIteratorIterator::CHILD_FIRST);
        foreach ($ri as $file) {
            $file->isDir() ? rmdir($file) : unlink($file);
        }
    }

    /**
     * Get PDO object
     *
     * @return null|PDO
     */
    final public function getPDO()
    {
        return self::$pdo;
    }

    /**
     * Remove "\r" and replace "\t" with spaces
     *
     * @param string $in
     *
     * @return mixed
     */
    public function normalizeString($in)
    {
        if (is_string($in)) {
            $in = str_replace("\r", '', $in);
            $in = str_replace("\t", '    ', $in);
        }
        return $in;
    }

    /**
     * Return source path
     *
     * @param Smarty_Internal_TemplateBase $tpl  template object
     * @param null|string                  $name optional template name
     * @param null|string                  $type optional template type
     * @param null|string                  $dir  optional template folder
     *
     * @return string
     * @throws \Exception
     */
    public function buildSourcePath($tpl, $name = null, $type = null, $dir = null)
    {
        $name = isset($name) ? $name : $tpl->source->name;
        $type = isset($type) ? $type : $tpl->source->type;
        $dir = isset($dir) ? $dir : $this->smarty->getTemplateDir(0);
        switch ($type) {
            case 'file':
            case 'php':
                return $this->normalizePath($dir . $name, true);
            case 'mysqltest':
            case 'mysql':
                return sha1($type . ':' . $name);
            case 'string':
                return sha1($name);
            default:
                throw new Exception("Unhandled source resource type '{$type}'");
        }
    }

    /**
     * Build template uid
     *
     * @param Smarty_Internal_TemplateBase $tpl  template object
     * @param null|string                  $value
     * @param null|string                  $name optional template name
     * @param null|string                  $type optional template type
     *
     * @return string
     * @throws \Exception
     */
    public function buildUid($tpl, $value = null, $name = null, $type = null)
    {
        $type = isset($type) ? $type : $tpl->source->type;
        $name = isset($name) ? $name : $tpl->source->name;
        switch ($type) {
            case 'file':
                if ($tpl instanceof Smarty) {
                    return sha1($this->normalizePath($this->smarty->getTemplateDir(0) . $name, true));
                }
                return sha1($tpl->source->filepath);
            case 'php':
                return sha1($tpl->source->filepath);
            case 'mysqltest':
            case 'mysql':
                return sha1($type . ':' . $name);
            case 'string':
                return sha1($name);
            default:
                throw new Exception("Unhandled source resource type '{$type}'");
        }
    }

    /**
     * Normalize file path
     *
     * @param  string $path input path
     * @param bool    $ds   if true use system directory separator
     *
     * @return string
     */
    public function normalizePath($path, $ds = false)
    {
        $path = str_replace(array('\\', '/./'), '/', $path);
        while ($path !== $new = preg_replace('#[^\.\/]+/\.\./#', '', $path)) {
            $path = $new;
        }
        if (DS !== '/' && $ds) {
            $path = str_replace('/', DS, $path);
        }
        return $path;
    }

    /**
     * Return base name
     *
     * @param \Smarty_Internal_Template|\Smarty_Internal_TemplateBase $tpl  template object
     * @param null|string                                             $name optional template name
     * @param null|string                                             $type optional template type
     *
     * @return null|string
     */
    public function getBasename(Smarty_Internal_Template $tpl, $name = null, $type = null)
    {
        $name = isset($name) ? $name : $tpl->source->name;
        $type = isset($type) ? $type : $tpl->source->type;
        switch ($type) {
            case 'file':
                if (($_pos = strpos($name, ']')) !== false) {
                    $name = substr($name, $_pos + 1);
                }
                return basename($name);
            case 'mysqltest':
            case 'mysql':
                return $name;
            case 'string':
                return '';
            default:
                return null;
        }
    }

    /**
     * Return compiled file path
     *
     * @param \Smarty_Internal_Template|\Smarty_Internal_TemplateBase $tpl        template object
     * @param bool                                                    $sub        use sub directory flag
     * @param bool                                                    $caching    caching flag
     * @param null|string                                             $compile_id optional compile id
     * @param null|string                                             $name       optional template name
     * @param null|string                                             $type       optional template type
     * @param null|string                                             $dir        optional template folder
     *
     * @return string
     * @throws \Exception
     */
    public function buildCompiledPath(Smarty_Internal_Template $tpl, $sub = true, $caching = false, $compile_id = null, $name = null, $type = null, $dir = null)
    {
        $sep = DS;
        $_compile_id = isset($compile_id) ? preg_replace('![^\w\|]+!', '_', $compile_id) : null;
        $sp = $this->buildSourcePath($tpl, $name, $type, $dir);
        $uid = $this->buildUid($tpl, $sp, $name, $type);
        $_flag = '';
        $_filepath = $uid . $_flag;
        // if use_sub_dirs, break file into directories
        if ($sub) {
            $_filepath = substr($_filepath, 0, 2) . $sep
                . substr($_filepath, 2, 2) . $sep
                . substr($_filepath, 4, 2) . $sep
                . $_filepath;
        }
        $_compile_dir_sep = $sub ? $sep : '^';
        if (isset($_compile_id)) {
            $_filepath = $_compile_id . $_compile_dir_sep . $_filepath;
        }
        // caching token
        if ($caching) {
            $_cache = '.cache';
        } else {
            $_cache = '';
        }
        $_compile_dir = $tpl->smarty->getCompileDir();
        // set basename if not specified
        $_basename = $this->getBasename($tpl, $name, $type);
        if ($_basename === null) {
            $_basename = basename(preg_replace('![^\w\/]+!', '_', $name));
        }
        // separate (optional) basename by dot
        if ($_basename) {
            $_basename = '.' . $_basename;
        }

        return $_compile_dir . $_filepath . '.' . $type . $_basename . $_cache . '.php';
    }

    /**
     * Return cache file path
     *
     * @param Smarty_Internal_TemplateBase $tpl        template object
     * @param bool                         $sub        use sub directory flag
     * @param null|string                  $cache_id   optional cache id
     * @param null|string                  $compile_id optional compile id
     * @param null|string                  $name       optional template name
     * @param null|string                  $type       optional template type
     * @param null|string                  $dir        optional template folder
     * @param null|string                  $cacheType  optional cache resource type
     *
     * @return string
     * @throws \Exception
     */
    public function buildCachedPath($tpl, $sub = true, $cache_id = null, $compile_id = null, $name = null, $type = null, $dir = null, $cacheType = null)
    {
        $cacheType = isset($cacheType) ? $cacheType : $tpl->smarty->caching_type;
        switch ($cacheType) {
            case 'file':$sep = DS;
                $_compile_id = isset($compile_id) ? preg_replace('![^\w\|]+!', '_', $compile_id) : null;
                $_cache_id = isset($cache_id) ? preg_replace('![^\w\|]+!', '_', $cache_id) : null;
                $sp = $this->buildSourcePath($tpl, $name, $type, $dir);
                $uid = $this->buildUid($tpl, $sp, $name, $type);
                $_filepath = $uid;
                // if use_sub_dirs, break file into directories
                if ($sub) {
                    $_filepath = substr($_filepath, 0, 2) . $sep
                        . substr($_filepath, 2, 2) . $sep
                        . substr($_filepath, 4, 2) . $sep
                        . $_filepath;
                }
                $_compile_dir_sep = $sub ? $sep : '^';
                if (isset($_cache_id)) {
                    $_cache_id = str_replace('|', $_compile_dir_sep, $_cache_id) . $_compile_dir_sep;
                } else {
                    $_cache_id = '';
                }
                if (isset($_compile_id)) {
                    $_compile_id = $_compile_id . $_compile_dir_sep;
                } else {
                    $_compile_id = '';
                }
                $smarty = isset($tpl->smarty) ? $tpl->smarty : $tpl;
                $_cache_dir = $smarty->getCacheDir();
                return $_cache_dir . $_cache_id . $_compile_id . $_filepath . '.' . basename($sp) . '.php';
            case 'mysql':
            case 'mysqltest':
            case 'pdo':
            case 'foobar':
                $sp = $this->buildSourcePath($tpl, $name, $type, $dir);
                $_compile_id = isset($compile_id) ? preg_replace('![^\w\|]+!', '_', $compile_id) : null;
                $_cache_id = isset($cache_id) ? preg_replace('![^\w\|]+!', '_', $cache_id) : null;
                return sha1($sp . $_cache_id . $_compile_id);
            default:
                throw new Exception("Unhandled cache resource type '{$cacheType}'");
        }
    }

    /**
     * Prefilter to remove \r from template source
     *
     * @param string $tpl_source
     * @param        $template
     *
     * @return mixed
     */
    function remove_cr($tpl_source, $template)
    {
        return str_replace(array("\r\n", "\r"), "\n", $tpl_source);
    }

    /**
     * Remove Smarty object from cached resources
     *
     */
    public function clearResourceCache()
    {
        if (class_exists('Smarty_Resource', false)) {
            if (isset(Smarty_Resource::$sources) && !empty(Smarty_Resource::$sources)) {
                foreach (Smarty_Resource::$sources as $obj) {
                    if (isset($obj->smarty)) {
                        $obj->smarty = null;
                    }
                }
                Smarty_Resource::$sources = array();
            }
            if (isset(Smarty_Resource::$compileds) && !empty(Smarty_Resource::$compileds)) {
                foreach (Smarty_Resource::$compileds as $obj) {
                    if (isset($obj->smarty)) {
                        $obj->smarty = null;
                    }
                }
                Smarty_Resource::$compileds = array();
            }
            if (isset(Smarty_Resource::$resources) && !empty(Smarty_Resource::$resources)) {
                foreach (Smarty_Resource::$resources as $obj) {
                    if (isset($obj->smarty)) {
                        $obj->smarty = null;
                    }
                }
                Smarty_Resource::$resources = array();
            }
        }
        if (class_exists('Smarty_CacheResource', false)) {
            if (isset(Smarty_CacheResource::$resources) && !empty(Smarty_CacheResource::$resources)) {
                foreach (Smarty_CacheResource::$resources as $obj) {
                    if (isset($obj->smarty)) {
                        $obj->smarty = null;
                    }
                }
                Smarty_CacheResource::$resources = array();
            }
        }
    }

    /**
     * Tears down the fixture
     * This method is called after a test is executed.
     *
     */
    protected function tearDown()
    {
        $this->clearResourceCache();
        if (isset($this->smarty->smarty)) {
            $this->smarty->smarty = null;
        }
        if (isset($this->smarty)) {
            $this->smarty = null;
        }
        if (isset($this->smartyBC->smarty)) {
            $this->smartyBC->smarty = null;
        }
        if (isset($this->smartyBC)) {
            $this->smartyBC = null;
        }
    }
}
