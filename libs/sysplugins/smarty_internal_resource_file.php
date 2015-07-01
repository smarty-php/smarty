<?php
/**
 * Smarty Internal Plugin Resource File
 *
 * @package    Smarty
 * @subpackage TemplateResources
 * @author     Uwe Tews
 * @author     Rodney Rehm
 */

/**
 * Smarty Internal Plugin Resource File
 * Implements the file system as resource for Smarty templates
 *
 * @package    Smarty
 * @subpackage TemplateResources
 */
class Smarty_Internal_Resource_File extends Smarty_Resource
{
    /**
     * build template filepath by traversing the template_dir array
     *
     * @param Smarty_Template_Source    $source    source object
     * @param  Smarty_Internal_Template $_template template object
     *
     * @return string fully qualified filepath
     * @throws SmartyException
     */
    protected function buildFilepath(Smarty_Template_Source $source, Smarty_Internal_Template $_template = null)
    {
        $file = $source->name;
        // absolute file ?
        if ($file[0] == '/' || $file[1] == ':') {
            $file = $source->smarty->_realpath($file);
            return is_file($file) ? $file : false;
        }
        // go relative to a given template?
        if ($file[0] == '.' && $_template && $_template->parent instanceof Smarty_Internal_Template && preg_match('#^[.]{1,2}[\\\/]#', $file)) {
            if ($_template->parent->source->type != 'file' && $_template->parent->source->type != 'extends' && !$_template->parent->allow_relative_path) {
                throw new SmartyException("Template '{$file}' cannot be relative to template of resource type '{$_template->parent->source->type}'");
            }
            $path = dirname($_template->parent->source->filepath) . DS . $file;
            // normalize path
            $path = $source->smarty->_realpath($path);
            // files relative to a template only get one shot
            return is_file($path) ? $path : false;
        }

        $_directories = $source->smarty->getTemplateDir(null, $source->isConfig);
        // template_dir index?
        if ($file[0] == '[' && preg_match('#^\[([^\]]+)\](.+)$#', $file, $fileMatch)) {
            $index = $fileMatch[1];
            $_directory = null;
            // try string indexes
            if (isset($_directories[$index])) {
                $_directory = $_directories[$index];
            } elseif (is_numeric($index)) {
                // try numeric index
                $index = (int) $index;
                if (isset($_directories[$index])) {
                    $_directory = $_directories[$index];
                } else {
                    // try at location index
                    $keys = array_keys($_directories);
                    $_directory = $_directories[$keys[$index]];
                }
            }
            if ($_directory) {
                $path = $_directory . $fileMatch[2];
                $path = $source->smarty->_realpath($path);
                if (is_file($path)) {
                    return $path;
                }
            } else {
                // index not found
                return false;
            }
        }

        // relative file name?
        foreach ($_directories as $_directory) {
            $path = $_directory . $file;
            if (is_file($path)) {
                return $source->smarty->_realpath($path);
            }
        }
        // Could be relative to cwd
        $path = $source->smarty->_realpath($file);
        if (is_file($path)) {
            return $path;
        }
        // Use include path ?
        if ($source->smarty->use_include_path) {
            return Smarty_Internal_Get_Include_Path::getIncludePath($_directories, $file, $source->smarty);
        }
        return false;
    }

    /**
     * test is file exists and save timestamp
     *
     * @param  Smarty_Template_Source $source source object
     * @param  string                 $file   file name
     *
     * @return bool                   true if file exists
     */
    protected function fileExists(Smarty_Template_Source $source, $file)
    {
        $source->timestamp = $source->exists = is_file($file);
        $source->timestamp = $source->exists ? filemtime($file) : false;
        return $source->exists;
    }

    /**
     * populate Source Object with meta data from Resource
     *
     * @param Smarty_Template_Source   $source    source object
     * @param Smarty_Internal_Template $_template template object
     */
    public function populate(Smarty_Template_Source $source, Smarty_Internal_Template $_template = null)
    {
        $source->filepath = $this->buildFilepath($source, $_template);

        if ($source->filepath !== false) {
            if (is_object($source->smarty->security_policy)) {
                $source->smarty->security_policy->isTrustedResourceDir($source->filepath, $source->isConfig);
            }
            $source->exists = true;
            $source->uid = sha1($source->filepath);
            if ($source->smarty->compile_check == 1) {
                $source->timestamp = filemtime($source->filepath);
            }
        } else {
            $source->timestamp = false;
            $source->exists = false;
        }
    }

    /**
     * populate Source Object with timestamp and exists from Resource
     *
     * @param Smarty_Template_Source $source source object
     */
    public function populateTimestamp(Smarty_Template_Source $source)
    {
        if (!$source->exists) {
            $source->timestamp = $source->exists = is_file($source->filepath);
        }
        if ($source->exists) {
            $source->timestamp = filemtime($source->filepath);
        }
    }

    /**
     * Load template's source from file into current template object
     *
     * @param  Smarty_Template_Source $source source object
     *
     * @return string                 template source
     * @throws SmartyException        if source cannot be loaded
     */
    public function getContent(Smarty_Template_Source $source)
    {
        if ($source->timestamp) {
            return file_get_contents($source->filepath);
        }
        if ($source instanceof Smarty_Template_Config) {
            throw new SmartyException("Unable to read config {$source->type} '{$source->name}'");
        }
        throw new SmartyException("Unable to read template {$source->type} '{$source->name}'");
    }

    /**
     * Determine basename for compiled filename
     *
     * @param  Smarty_Template_Source $source source object
     *
     * @return string                 resource's basename
     */
    public function getBasename(Smarty_Template_Source $source)
    {
        return basename($source->filepath);
    }
}
