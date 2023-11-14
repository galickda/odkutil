# make datatable aware
.datatable.aware <- TRUE

## Class for ODKForm
setClass('XLSForm', slots=list(path='character', survey='data.table',
                               repeats='data.table', choices='data.table',
                               combine_views='data.table', settings='data.table'))

######################
### Set Generic methods for class
setGeneric('processForm', function(.Object, ...){
  standardGeneric('processForm')
})
setGeneric('calculateTypes', function(.Object, ...){
  standardGeneric('calculateTypes')
})
setGeneric('addInstanceID', function(.Object, ...){
  standardGeneric('addInstanceID')
})
setGeneric('configureViews', function(.Object, ...){
  standardGeneric('configureViews')
})
setGeneric('genSQLBase', function(.Object, ...){
  standardGeneric('genSQLBase')
})
setGeneric('genSQLRepeatLevel1', function(.Object, ...){
  standardGeneric('genSQLRepeatLevel1')
})
setGeneric('genSQLRepeatLevel1_combine', function(.Object, ...){
  standardGeneric('genSQLRepeatLevel1_combine')
})
setGeneric('genSQLRepeatLevel2', function(.Object, ...){
  standardGeneric('genSQLRepeatLevel2')
})
setGeneric('subsetView', function(.Object, ...){
  standardGeneric('subsetView')
})
setGeneric('genSQL', function(.Object, ...){
  standardGeneric('genSQL')
})
setGeneric('genSQLAll', function(.Object, ...){
  standardGeneric('genSQLAll')
})
setGeneric('genFullViewNames', function(.Object, ...){
  standardGeneric('genFullViewNames')
})
setGeneric('genPermissionsSQL', function(.Object, ...){
  standardGeneric('genPermissionsSQL')
})
setGeneric('genCodebook', function(.Object, ...){
  standardGeneric('genCodebook')
})

########################################
### Set of functions to initialize
##  as part of initialization we want to process the form and include instanceID

# append the instanceID field to the form
setMethod('addInstanceID', 'XLSForm', function(.Object){
  # add instanceid into the form
  .Object@survey=rbind(.Object@survey,
                       data.table::data.table(type='text', name='instanceID',
                            repeat_indicator=0, group_indicator=0,
                            node_name=list(c('data', 'meta', 'instanceID')),
                            repeat_depth=0,
                            repeat_parent=NA), fill=T)
  return(.Object)
})


setMethod('processForm', 'XLSForm',
          function(.Object){
  .Object@survey[,`:=` ('type'=stringr::str_trim(type),
                        'name'=stringr::str_trim(name))]


  # remove notes and end
  .Object@survey=.Object@survey[!type %in% c('note', 'end')]

  #### create path to object
  # first create columns to know when repeat or group is starting/ending
  .Object@survey[, repeat_indicator:=data.table::fcase(grepl('begin repeat|begin_repeat', type), 1,
                                           grepl('end repeat|end_repeat', type), -1,
                                           default=0)]
  .Object@survey[, group_indicator:=data.table::fcase(grepl('begin group|begin_group', type), 1,
                                          grepl('end group|end_group', type), -1,
                                          default=0)]

  # Create a column with a list that has the path to the node
  node_name=vector(mode='character', nrow(.Object@survey))
  thisnode='data'
  for(i in 1:nrow(.Object@survey)){
    if(.Object@survey$repeat_indicator[i]==1|.Object@survey$group_indicator[i]==1){
      thisnode=c(thisnode, .Object@survey$name[i])
      node_name[i]=list(thisnode)
    } else if(.Object@survey$repeat_indicator[i]==-1|.Object@survey$group_indicator[i]==-1){
      thisnode=head(thisnode, -1)
      node_name[i]=list(thisnode)
    } else {
      node_name[i]=list(c(thisnode, .Object@survey$name[i]))
    }
  }
  .Object@survey$node_name=node_name

  # Now create a repeat depth column (each cascading repeat needs another view)
  .Object@survey[, repeat_depth:=cumsum(repeat_indicator)]
  # And another with the name of the parent repeat (if repeat inside a repeat)
  .Object@survey[repeat_depth>0,
                 repeat_parent:=zoo::na.locf(data.table::fifelse(repeat_indicator==1, name, as.character(NA)))]
  .Object@survey[repeat_depth==1, # fix nodes after exiting level-2 repeat
                 repeat_parent:=zoo::na.locf(data.table::fifelse(repeat_indicator==1, name, as.character(NA)))]
  .Object@survey[repeat_indicator==1, # fix parent for begin repeats
                 repeat_parent:=unlist(node_name[[1]][length(node_name[[1]])-1]), by=name]
  .Object@survey[repeat_indicator==1 & repeat_depth==1, # set parent to NA for 1st level repeats
                 repeat_parent:=NA]
  .Object <- addInstanceID(.Object)

  # set the repeats attribute
  .Object@repeats <- .Object@survey[repeat_indicator==1 | group_indicator==1]
  # remove begin/end repeats and groups from the survey object
  .Object@survey <- .Object@survey[group_indicator==0 & repeat_indicator==0]

  # add a column for name of the choice list and another if is select_multiple
  .Object@survey[grepl('select_multiple|select_one', type),
                 choicelist:=stringr::str_trim(gsub('select_multiple *|select_one *', '', type))]
  .Object@survey[,select_multiple:=grepl('select_multiple', type)]

  ########
  # add various paths to use in sql queries
  ## direct path (use for anything outside repeat)
  .Object@survey[,path:=paste0(' as_json -> \'',
                               paste(head(unlist(node_name), -1),
                                     collapse='\' -> \''),
                               '\' ->> \'',
                               tail(unlist(node_name), 1),
                               '\''),
                 by=name]
  ## path for repeat -- use with jsonb_to_recordset()
  .Object@repeats[,path_repeat:=paste0(' as_json -> \'',
                                       paste(head(unlist(node_name), -1),
                                             collapse='\' -> \''),
                                       '\' -> \'',
                                       tail(unlist(node_name), 1),
                                       '\''),
                  by=name]

  ## column for the direct parent
  .Object@survey[,direct_parent:=unlist(node_name[[1]][length(node_name[[1]])-1]), by=name]

  ## relative path from repeat_parent
  .Object@survey[,repeat_relative_path:=sapply(strsplit(path, paste0(repeat_parent, '\'')), '[', 2),
                 by=name]
  ## relative path from repeat_parent as json (for 2nd level repeats)
  .Object@repeats[,repeat_relative_path_asjson:=sapply(strsplit(path_repeat, paste0(repeat_parent, '\'')), '[', 2),
                 by=name]


  # change single to double quotes and remove leading '->' for use with laterals
  path_splits=lapply(gsub("^[^\']*", "", .Object@survey$repeat_relative_path),
                     strsplit, split='->')
  new_paths=sapply(path_splits, function(s){
    ifelse(length(s[[1]])>1,
           paste(c(gsub('\'', '\"', s[[1]][1]),
                   s[[1]][2:length(s[[1]])]),
                 collapse='->'),
           gsub('\'', '\"', s[[1]])
    )
  })
  .Object@survey[,lateral_path:=stringr::str_trim(new_paths)]

  .Object@survey[,newname:=tolower(name)]
  return(.Object)
})

setMethod('configureViews', 'XLSForm',
          function(.Object, include_cols=NULL, change_names=NULL){
            # verify combine_views
            if(nrow(.Object@combine_views)>0){
              needcols=c('repeat_name', 'view', 'name_old', 'name_new', 'repeat_id')
              if(!all(needcols %in% colnames(.Object@combine_views))){
                missingcol=which(!needcols %in% colnames(.Object@combine_views))
                stop(paste('combine_views does not contain column(s):', paste(missingcol, collapse=', ')))
              }
              name_mismatch=.Object@combine_views[!name_old %in% .Object@survey[,name], unique(name_old)]
              if(length(name_mismatch)>0){
                stop(paste('Mismatching column names found in combine_views:\n',
                           paste(name_mismatch, collapse=', ')))
              }
            }
            if(!is.null(include_cols)){
              name_mismatch_include=names(include_cols)[which(!names(include_cols) %in% .Object@survey[,name])]
              if(length(name_mismatch_include)>0){
                stop(paste('Mismatching column names found in include_cols:\n',
                           paste(name_mismatch_include, collapse=', ')))
              }
            }

            # merge in any nodes in a repeat in combine_views but not there yet
            if(nrow(.Object@combine_views)>0){
              .Object@combine_views <- data.table::rbindlist(list(
                .Object@survey[!name %in% .Object@combine_views[,name_old] &
                                 repeat_parent %in% .Object@combine_views[,unique(repeat_name)],
                               .(repeat_name_col=.Object@combine_views[repeat_name %in% repeat_parent,
                                                                       unique(repeat_name_col)][1],
                                 repeat_name=repeat_parent,
                                 view=data.table::fifelse(repeat_parent %in% .Object@combine_views[,unique(view)],
                                              repeat_parent,
                                              .Object@combine_views[repeat_name %in% repeat_parent, unique(view)][1]),
                                 name_old=name,
                                 name_new=tolower(name),
                                 repeat_id=.Object@combine_views[repeat_name %in% repeat_parent,
                                                                 unique(repeat_id)]),
                               by=repeat_parent][,!"repeat_parent"],
                .Object@combine_views))
            } else {
              .Object@combine_views <- data.table::data.table(repeat_name_col=character(),
                                                              repeat_name=character(),
                                                              view=character(),
                                                              name_old=character(),
                                                              name_new=character(),
                                                              repeat_id=character())
            }

            ## PART I: VIEWS (what view should a node in a repeat be part of)
            if(nrow(.Object@combine_views)>0){
              .Object@survey[.Object@combine_views, on='name==name_old',
                             repeat_view:=i.view]
            } else {
              .Object@survey[,repeat_view:=as.character(NA)]
            }

            .Object@survey[is.na(repeat_view), repeat_view:=repeat_parent]

            ## PART II: create columns to specify which nodes to include in each view
            .Object@survey[,include_base:=(repeat_depth==0)]

            # now do the repeat views
            # for include_cols: value should be always or the name of a view
            repeat_views= .Object@survey[repeat_depth>0,unique(repeat_view)]
            for(v in repeat_views){
              data.table:::set(.Object@survey, j=paste0('include_', v),
                               value = apply(.Object@survey, 1,
                                              function(r){
                                                n=as.character(r['name'])
                                                if (r['repeat_view'] %in% c(v)) {
                                                  T
                                                } else if (length(include_cols)==0 | !n %in% names(include_cols)){
                                                  F
                                                } else{
                                                  any(c('always', v) %in% unlist(include_cols[n]))
                                                }
                                              }))
            }


            # fix the columns that are in combine_views
            if(nrow(.Object@combine_views)>0){
              .Object@survey[.Object@combine_views, on='name==name_old',
                             newname:=i.name_new]
            }
            # renaming newname for the query as specified
            .Object@survey[name %in% names(change_names),
                           newname:=change_names[name]]
            return(.Object)
          })



# Main initializer for the class
setMethod('initialize', 'XLSForm',
          function(.Object, path,
                   combine_views=data.table::data.table(), include_cols=list('instanceID'='always'),
                   change_names=NULL, exclude_cols=NULL){
  .Object@path <- path
  surv=data.table::data.table(openxlsx::read.xlsx(path, sheet='survey'))
  coltoget=grep('type|name|label', colnames(surv), value=T)
  .Object@survey <- surv[,..coltoget]
  if(!is.null(exclude_cols)){
    .Object@survey <- .Object@survey[!name %in% exclude_cols]
  }
  choices <- data.table::data.table(openxlsx::read.xlsx(path, sheet='choices'))
  choicecols = grep('list_name|name|label', colnames(choices), value=T)
  .Object@choices <- choices[,..choicecols]
  .Object@combine_views <- combine_views
  .Object <- processForm(.Object)
  .Object <- configureViews(.Object, include_cols=include_cols, change_names=change_names)
  settings <- data.table::data.table(openxlsx::read.xlsx(path, sheet='settings'))
  if(!'form_id' %in% colnames(settings)){
    stop('form_id not specified in settings tab of XLSForm')
  }
  .Object@settings <- settings
  return(.Object)
})


# calculate the correct type for postgres to use
setMethod('calculateTypes', 'XLSForm',
          function(.Object, calc_types=NULL, calc_default='text', type_default='text'){
            # parse the choices to return select_ones where we can't use integer
            .Object@choices[,name.int:=as.integer(name)]
            choiceslists=.Object@choices[!is.na(list_name),.(coercable=.SD[is.na(name.int),.N]==0),by=list_name]
            choiceslists[grepl('customize_me', list_name), coercable:=F]
            choices_usetext=choiceslists[coercable==F,list_name]

            # define types to use for postgres
            .Object@survey[,postgres_type:=data.table::fcase(grepl('select_multiple', type), 'text',
                                                 grepl(paste(choices_usetext, collapse='|'), type), 'text',
                                                 grepl('integer|select_one', type), 'int',
                                                 grepl('float', type), 'numeric',
                                                 grepl('calculate', type),
                                                 data.table::fifelse(name %in% names(calc_types),
                                                                     as.character(calc_types[name]),
                                                                     calc_default),
                                                 default=type_default)]

          })

setMethod('subsetView', 'XLSForm', function(.Object, repeat_view_name){
  .Object@survey <- .Object@survey[get(paste0('include_', repeat_view_name))==T]
  return(.Object)
})

#' Create ODK form object
#'
#' This function creates an object containing all three tabs of the ODK XLSForm indicated,
#' calculates the data type for each variable, and splits the form into views based on
#' `combine_views` and/or the repeats in the ODK form. Returns an object of class `XLSForm`.
#'
#' @param path Path to the ODK XLSForm (must be an xlsx file with the following sheets: survey, choices, settings)
#' @param combine_views `data.table` object used to configure questions that are in different repeats
#' but should be combined into the same column in a single view. Should be a data.table with the following columns:
#' - `repeat_name_col`: the name of the column to be used to identify which repeat a value comes from.
#' - `repeat_name`: Name of the repeat as programmed in the ODK XLSForm
#' - `view`: Name of the view this question should be included in
#' - `name_old`: Name of the question as programmed in the ODK XLSForm
#' - `name_new`: Name of the column that should contain this question in the view
#' - `repeat_id`: Name of the repeat as it should be entered in the column defined by `repeat_name_col`
#' @param include_cols List mapping origin question names to (character vector of) views these questions should be included in.
#' Values should be `'always'` or the name of a repeat or view as defined in `combine_views` and/or the ODK XLSForm.
#' @param change_names Character vector mapping original question names to the names that should be used
#' for the SQL view column names.
#' @param exclude_cols Character vector listing the names of questions that should be excluded from the view.
#' Lines in the XLSForm with type note, or which begin/end a repeat or group are automatically excluded as separate variables from SQL views.
#' @param calc_types Character vector mapping the name of calculate fields to the type to be used in the SQL statement.
#' Calculate fields not present in this vector default to `calc_default`.
#' @param calc_default The default type to be used for calculate fields, when they do not appear in `calc_types`.
#' @param type_default The default type to be used for questions if data type cannot be guessed
#'
#' @returns An S4 object of class `XLSForm`
#' @aliases createODK
#' @export

createODK = function(path, combine_views=data.table::data.table(),
                      include_cols=NULL, change_names=NULL,
                      exclude_cols=NULL, calc_types=NULL,
                      calc_default='text', type_default='text'){
  form=new('XLSForm', path=path, combine_views=combine_views,
           include_cols=include_cols, change_names=change_names,
           exclude_cols=exclude_cols)
  calculateTypes(form, calc_types=calc_types,
                  calc_default=calc_default, type_default=type_default)
  return(form)
}







#####################################################################
###################### GENERATING SQL QUERIES #######################

setMethod('genSQLBase', 'XLSForm',
          function(.Object, schema, view_name, startdate, enddate,
                   view_prefix='', filter_version_sql=F,
                   source_table='openhds.form_submission',
                   distinct_on=NULL, sm_exclude=vector('character', length=0)){
            # get form id and version from the object
            form_id=.Object@settings[1,form_id]
            if('version' %in% colnames(.Object@settings)){
              form_version=.Object@settings[1,version]
            } else{
              form_version=NA
            }
            if(filter_version_sql==T & is.na(form_version)){
              stop("To filter by form version form_version must be specified")
            }
            # trim trailing _ from view name
            if(substr(view_name, nchar(view_name), nchar(view_name)) %in% c('_', '-')){
              view_name=substr(view_name, 1, nchar(view_name)-1)
            }
            # parse the distinct on
            distinct_on_sql=paste0('DISTINCT ON (',
                                   paste(.Object@survey[name %in% distinct_on, path],
                                         collapse=',\n'),
                                   ')\n')
            # generate the sql
            paste0('CREATE OR REPLACE VIEW \"', schema, '\".', view_prefix, view_name,
                   ' AS \nSELECT ',
                   ifelse(!is.null(distinct_on),
                          distinct_on_sql,
                          ''),
                   paste(data.table::fcase(
                     # select_multiples
                     .Object@survey[include_base==T,
                                    grepl('select_multiple', type) & !name %in% sm_exclude],
                     apply(.Object@survey[include_base==T], 1, function(row){
                       paste(sapply(.Object@choices[list_name %in% row['choicelist'], name],
                                    function(choice){
                                      paste0('(', 'CASE WHEN ', row['path'],
                                             ' ~ ', '\'\\m(', choice, ')\\M\' ',
                                             'THEN 1 \nWHEN ', row['path'],
                                             ' IS NOT NULL',
                                             '\nAND ',
                                             row['path'], '!=\'\' ',
                                             'THEN 0 \nELSE NULL END)::int as ',
                                             row['newname'], '_', choice)
                                      }), collapse=',\n')}),
                     # GPS coordinates
                     .Object@survey[include_base==T, grepl('geopoint', type)],
                     paste0('st_setsrid(st_point(split_part((',
                            .Object@survey[include_base==T,path], ')::text, \' \'::text, 2)::numeric::double precision, split_part((',
                            .Object@survey[include_base==T,path], ')::text, \' \'::text, 1)::numeric::double precision), 4326) ',
                            'as ', .Object@survey[include_base==T,newname]),
                     # not select multiples, or geopoints, or select_multiple that were excluded
                     .Object@survey[include_base==T,!(grepl('geopoint|select_multiple', type) |
                                                         name %in% sm_exclude)],
                     paste0(ifelse(.Object@survey[include_base==T,postgres_type %in% c('numeric', 'int')],
                                   paste0('(CASE WHEN ',
                                          .Object@survey[include_base==T, path],
                                          '=\'\' THEN NULL ELSE '),
                                   '('),
                            .Object@survey[include_base==T, path],
                            ifelse(.Object@survey[include_base==T,postgres_type %in% c('numeric', 'int')],
                                   ' END ',
                                   ''),
                             ')::', .Object@survey[include_base==T,postgres_type],
                             ' as ',
                             .Object@survey[include_base==T,newname])),
                    collapse=',\n'),
                    '\nFROM ', source_table,
                   '\nWHERE form_id=\'', form_id, '\'',
                   ifelse(filter_version_sql==T,
                          paste0(' AND form_version=\'', form_version, '\''),
                          ''),
                   ifelse(!is.null(startdate),
                          paste0('\nAND collected::date >= \'', format(startdate, '%Y-%m-%d'),'\''),
                          ''),
                   ifelse(!is.null(enddate),
                          paste0('\nAND collected::date < \'', format(enddate, '%Y-%m-%d'), '\';'),
                          ''))
          })


# query for 1st level repeat without combining any repeats
setMethod('genSQLRepeatLevel1_combine', 'XLSForm',
          function(.Object, schema, view_name, startdate, enddate,
                   view_prefix='', filter_version_sql=F,
                   source_table='openhds.form_submission',
                   distinct_on=NULL, sm_exclude=vector('character', length=0)){
            # get form id and version from the object
            form_id=.Object@settings[1,form_id]
            if('version' %in% colnames(.Object@settings)){
              form_version=.Object@settings[1,version]
            } else{
              form_version=NA
            }
            # parse the distinct on
            distinct_on_sql=paste0('DISTINCT ON (',
                                   paste(.Object@survey[name %in% distinct_on, path],
                                         collapse=',\n'),
                                   ')\n')
            # save the full object to another variable and subset to the relevant view
            .Object=subsetView(.Object, view_name)
            # what are the repeats we are combining
            repeats=.Object@survey[!is.na(repeat_parent),unique(repeat_parent)]
            view_cols=.Object@survey[repeat_depth>0,unique(newname)]
            # generate the sql
            paste0('CREATE OR REPLACE VIEW \"', schema, '\".', view_prefix, view_name,' AS\n',
                   paste(sapply(repeats, function(rpt){
                     paste0('SELECT ',
                            ifelse(!is.null(distinct_on),
                                   distinct_on_sql,
                                   ''),
                            # things outside the repeat
                            paste(ifelse(.Object@survey[repeat_depth==0, !is.na(choicelist) & select_multiple==T & !name %in% sm_exclude],
                                         # the select_multiples
                                         apply(.Object@survey[repeat_depth==0], 1, function(row){
                                           paste(sapply(.Object@choices[list_name %in% row['choicelist'], name],
                                                        function(choice){
                                                          paste0('(CASE WHEN ', .Object@survey[repeat_depth==0, path],
                                                                 ' ~ \'\\m(', choice, ')\\M\' THEN 1',
                                                                 '\nWHEN ', .Object@survey[repeat_depth==0, path], 'IS NOT NULL',
                                                                 '\nAND ', .Object@survey[repeat_depth==0, path], '!=\'\'',
                                                                 'THEN 0',
                                                                 '\nELSE NULL END)::int as', .Object@survey[repeat_depth==0, newname])
                                                        }),
                                                 collapse='\n')
                                         }),
                                         # not a select_multiple
                                         paste('(', .Object@survey[repeat_depth==0, path],
                                               ')::', .Object@survey[repeat_depth==0, postgres_type],
                                               'as',
                                               .Object@survey[repeat_depth==0, newname])
                            ),
                            collapse=',\n'), ',\n',
                            # repeat_name
                            paste0('\'', .Object@combine_views[repeat_name==rpt,unique(repeat_id)],
                                   '\' as ', .Object@combine_views[repeat_name==rpt,unique(repeat_name_col)],',\n'),
                            # inside the repeat
                            paste(
                              sapply(view_cols, function(vc){
                                node=.Object@survey[repeat_parent==rpt & newname==vc]
                                if(nrow(node)>0){
                                  if(nrow(node)>1){
                                    stop(paste0("Multiple rows cannot be mapped to the same column name (",
                                                vc, ")"))
                                  }
                                  if(node[,!is.na(choicelist) & select_multiple==T & !name %in% sm_exclude]){ #select_multiple
                                    column_def=paste(sapply(.Object@choices[list_name %in% node$choicelist,name],
                                                            function(choice){
                                                              colpath=paste0('l_', rpt, '.', node[,lateral_path])
                                                              paste0('(CASE WHEN ', colpath,
                                                                     ' ~ \'\\m(', choice, ')\\M\'', ' THEN 1',
                                                                     '\nWHEN ', colpath, ' IS NOT NULL',
                                                                     '\nAND ', colpath, '!=\'\' THEN 0',
                                                                     '\nELSE NULL END)::int as ',
                                                                     vc, '_', choice)
                                                            }),
                                                     collapse=',\n')
                                  } else {
                                    column_def=paste0(ifelse(node[,postgres_type %in% c('numeric', 'int')],
                                                              paste0('(CASE WHEN l_', rpt, '.',
                                                                     node[,lateral_path],
                                                                     '=\'\' THEN NULL ELSE ',
                                                                     'l_', rpt, '.', node[,lateral_path],
                                                                     ' END'),
                                                              paste0('(l_', rpt, '.', node[,lateral_path])),
                                                      ')::',
                                                      node[,postgres_type],
                                                      ' as ', vc)
                                  }
                                } else {
                                  node=.Object@survey[newname==vc]
                                  if (node[, any(!is.na(choicelist) & select_multiple==T & !name %in% sm_exclude)]){
                                    if(node[,.N,by=choicelist][,.N>1]){
                                      stop("Rows being collapse to the same variable must have the same choicelist")
                                    }
                                    column_def=paste(
                                      sapply(.Object@choices[list_name %in% node[,unique(choicelist)],name],
                                             function(choice){paste0('(NULL) AS ', vc, '_', choice)
                                               }),
                                      collapse=',\n')
                                  } else {
                                    column_def=paste0('(NULL) as ', vc)
                                  }
                                }
                                column_def
                              }),
                              collapse=',\n'),
                            '\nFROM openhds.form_submission,\n',
                            'LATERAL jsonb_to_recordset(',
                            .Object@repeats[name == rpt, path_repeat],
                            ') l_', rpt, '(',
                            paste(paste0(
                              '\"',
                              c(.Object@survey[direct_parent==rpt, name],
                                .Object@survey[repeat_depth == 1 &
                                                 repeat_parent!=direct_parent &
                                                 repeat_parent==rpt,
                                               unique(direct_parent)]),
                              '\" ',
                              c(rep('text', .Object@survey[direct_parent==rpt, .N]),
                                rep('jsonb', .Object@survey[repeat_depth == 1 &
                                                              repeat_parent!=direct_parent &
                                                              repeat_parent==rpt, length(unique(direct_parent))]))
                            ),
                            collapse = ' ,'),
                            ')\n',
                            'WHERE form_id=\'', form_id, '\'',
                            ifelse(filter_version_sql==T,
                                   paste0(' AND form_version=\'', form_version, '\''),
                                   ''),
                            ifelse(!is.null(startdate),
                                   paste0('\nAND collected::date >= \'', format(startdate, '%Y-%m-%d'),'\''),
                                   ''),
                            ifelse(!is.null(enddate),
                                   paste0('\nAND collected::date < \'', format(enddate, '%Y-%m-%d'), '\';'),
                                   ''))
                   })))
          })



# query for 1st level repeat without combining any repeats
setMethod('genSQLRepeatLevel1', 'XLSForm',
          function(.Object, schema, view_name, startdate, enddate,
                   view_prefix='', filter_version_sql=F,
                   source_table='openhds.form_submission',
                   distinct_on=NULL, sm_exclude=vector('character', length=0)){
            # get form id and version from the object
            form_id=.Object@settings[1,form_id]
            if('version' %in% colnames(.Object@settings)){
              form_version=.Object@settings[1,version]
            } else{
              form_version=NA
            }
            # parse the distinct on
            distinct_on_sql=paste0('DISTINCT ON (',
                                   paste(.Object@survey[name %in% distinct_on, path],
                                         collapse=',\n'),
                                   ')\n')
            # subset to the relevant view
            .Object=subsetView(.Object, view_name)
            # generate the sql
            paste0('CREATE OR REPLACE VIEW \"', schema, '\".', view_prefix, view_name,
                   ' AS ',
                   '\nSELECT ',
                   ifelse(!is.null(distinct_on),
                          distinct_on_sql,
                          ''),
                   paste(ifelse(.Object@survey[repeat_depth==0, !is.na(choicelist) & select_multiple==T & !name %in% sm_exclude],
                                #the select_multiples
                                apply(.Object@survey[repeat_depth==0], 1, function(row){
                                  paste(sapply(.Object@choices[list_name %in% row['choicelist'], name],
                                               function(choice){
                                                 paste0('(CASE WHEN ', row[,path],
                                                        ' ~ \'\\m(', choice, ')\\M\' THEN 1',
                                                        '\nWHEN ', .Object@survey[repeat_depth==0, path], 'IS NOT NULL',
                                                        '\nAND ', .Object@survey[repeat_depth==0, path], '!=\'\'',
                                                        'THEN 0',
                                                        '\nELSE NULL END)::int as', .Object@survey[repeat_depth==0, newname])
                                               }),
                                        collapse='\n')
                                }),
                                # not a select_multiple
                                paste('(', .Object@survey[repeat_depth==0, path],
                                      ')::', .Object@survey[repeat_depth==0, postgres_type],
                                      'as',
                                      .Object@survey[repeat_depth==0, newname])
                   ),
                   collapse=',\n'), ',\n',
                   paste(ifelse(.Object@survey[repeat_depth==1, !is.na(choicelist) & select_multiple==T & !name %in% sm_exclude],
                                # the select_multiples
                                apply(.Object@survey[repeat_depth==1], 1, function(row){
                                  paste(
                                    sapply(.Object@choices[list_name==row['choicelist'], name],
                                           function(choice){
                                             colpath=paste0('r.', row['lateral_path'])
                                             paste0('(CASE WHEN ', colpath,
                                                    ' ~ \'\\m', choice, '\\M\'', ' THEN 1',
                                                    'WHEN ', colpath, 'IS NOT NULL',
                                                    '\nAND ', colpath, '!=\'\' THEN 0',
                                                    '\nELSE NULL END)::int as ',
                                                    row['newname'], '_', choice)
                                         }),
                                    collapse=',\n')
                                }),
                                # not a select multiple
                                paste0(ifelse(.Object@survey[repeat_depth==1,postgres_type %in% c('numeric', 'int')],
                                              paste0('(CASE WHEN r.',
                                                     .Object@survey[repeat_depth==1,lateral_path],
                                                     '=\'\' THEN NULL ELSE '),
                                              '('),
                                       'r.',
                                       .Object@survey[repeat_depth==1,lateral_path],
                                       data.table::fifelse(.Object@survey[repeat_depth==1,postgres_type %in% c('numeric', 'int')],
                                              ' END ',
                                              ''),
                                       ')::', .Object@survey[repeat_depth==1,postgres_type],
                                       ' as ',
                                       .Object@survey[repeat_depth==1,newname])
                                ),
                         collapse=',\n'),
                   '\nFROM openhds.form_submission,',
                   '\nLATERAL jsonb_to_recordset(',
                   .Object@repeats[name==view_name,
                                   path_repeat],
                   ')
                          r(',
                   paste(
                     paste(
                       paste0('\"',
                              c(.Object@survey[repeat_depth==1 &
                                                 repeat_parent==direct_parent,
                                               name],
                                .Object@survey[repeat_depth==1 &
                                                 repeat_parent!=direct_parent,
                                               unique(direct_parent)]),
                              '\"'),
                       paste(c(.Object@survey[repeat_depth==1 &
                                                repeat_parent==direct_parent,
                                              # set numeric to text to avoid conversion errors
                                              # (coerce above in the query instead)
                                              data.table::fifelse(postgres_type %in% c('numeric', 'int'), 'text', postgres_type)],
                               .Object@survey[repeat_depth==1 &
                                                repeat_parent!=direct_parent,
                                              rep('jsonb', length(unique(direct_parent)))]
                       ))
                     ),
                     collapse=', '),
                   ')
                          WHERE form_id=\'', form_id, '\'',
                   ifelse(filter_version_sql==T,
                          paste0(' AND form_version=\'', form_version, '\''),
                          ''),
                   ifelse(!is.null(startdate),
                          paste0('\nAND collected::date >= \'', format(startdate, '%Y-%m-%d'),'\''),
                          ''),
                   ifelse(!is.null(enddate),
                          paste0('\nAND collected::date < \'', format(enddate, '%Y-%m-%d'), '\';'),
                          ''))
          })



setMethod('genSQLRepeatLevel2', 'XLSForm',
          function(.Object, schema, view_name, startdate, enddate,
                   view_prefix='', filter_version_sql=F,
                   source_table='openhds.form_submission',
                   distinct_on=NULL){
            # get form id and version from the object
            form_id=.Object@settings[1,form_id]
            if('version' %in% colnames(.Object@settings)){
              form_version=.Object@settings[1,version]
            } else{
              form_version=NA
            }
            parent=.Object@repeats[name %in% .Object@survey[repeat_depth==2,repeat_parent],
                        unique(repeat_parent)]
            if(length(parent)>1){
              stop(paste0('Could not create view ',
                          'mis_', mis_year, '_', v, '\n',
                          'Two second-level repeats with different repeat parents cannot be combined'))
            }
            if(nrow(.Object@combine_views)>0){
              view_cols=.Object@combine_views[view==view_name,unique(name_new)]
            } else{
              view_cols=NULL
            }
            # parse the distinct on
            distinct_on_sql=paste0('DISTINCT ON (',
                                   paste(.Object@survey[name %in% distinct_on, path],
                                         collapse=',\n'),
                                   ')\n')
            # subset to the relevant view
            .Object=subsetView(.Object, view_name)
            # generate the sql (subquery with parent repeat elements)
            subquery=paste0('WITH repeat AS (SELECT ',
                            ifelse(!is.null(distinct_on),
                                   distinct_on_sql,
                                   ''),
                            paste0(paste('(', .Object@survey[repeat_depth==0, path],
                                        ')::', .Object@survey[repeat_depth==0, postgres_type],
                                        'as',
                                        .Object@survey[repeat_depth==0, newname]),
                                  collapse=',\n'), ',\n',
                            'jsonb_array_elements(',
                            .Object@repeats[name==parent,path_repeat],
                            ') as element
                                     FROM ', source_table,
                                     '\nWHERE form_id=\'', form_id, '\'',
                            ifelse(filter_version_sql==T,
                                   paste0(' AND form_version=\'', form_version, '\''),
                                   ''),
                            ifelse(!is.null(startdate),
                                   paste0('\nAND collected::date >= \'', format(startdate, '%Y-%m-%d'),'\''),
                                   ''),
                            ifelse(!is.null(enddate),
                                   paste0('\nAND collected::date < \'', format(enddate, '%Y-%m-%d'), '\''),
                                   ''),
                            ')\n')
            mainquery=paste(sapply(.Object@survey[repeat_depth==2,unique(repeat_parent)],
                                   function(rpt){
                                     if(is.null(view_cols) | !rpt %in% .Object@combine_views$repeat_name){ # we aren't combining with other views
                                       cols_to_pull=paste(
                                         # nodes in the parent repeat
                                         c(paste('element',
                                                 .Object@survey[repeat_depth==1,repeat_relative_path],
                                                 'as',
                                                 .Object@survey[repeat_depth==1,newname]),
                                           # nodes in the level-2 repeat
                                           paste0('(l_', rpt, '.',
                                                  .Object@survey[repeat_parent==rpt,lateral_path],
                                                  ')::', .Object@survey[repeat_parent==rpt,postgres_type],
                                                  ' as ',
                                                  .Object@survey[repeat_parent==rpt,newname])),
                                         collapse=',\n')
                                     } else {
                                       cols_to_pull=paste(
                                         # nodes in the parent repeat
                                         c(paste0('(element',
                                                  .Object@survey[repeat_depth==1,repeat_relative_path],
                                                  ')::', .Object@survey[repeat_depth==1,postgres_type],
                                                  ' as ',
                                                  .Object@survey[repeat_depth==1,newname]),
                                           # name of repeat (we are unioning different repeats)
                                           paste0('\'', .Object@combine_views[view==view_name & repeat_name==rpt,
                                                                              unique(repeat_id)][1],
                                                  '\' as ', .Object@combine_views[view==view_name & repeat_name==rpt,
                                                                                  unique(repeat_name_col)][1]),
                                           # nodes in the level-2 repeat
                                           sapply(view_cols, function(c){
                                             thispath=.Object@survey[repeat_parent==rpt & newname==c,
                                                                lateral_path]
                                             paste0(ifelse(length(thispath)>0,
                                                           paste0('(', 'l_', rpt, '.',
                                                                  thispath,
                                                                  ')::',
                                                                  .Object@survey[repeat_parent==rpt & newname==c,
                                                                            postgres_type]),
                                                           'NULL'),
                                                    ' as ', c)
                                           })),
                                         collapse=',\n')
                                     }
                                     paste0('SELECT ',
                                            paste(.Object@survey[repeat_depth==0, newname],
                                                  collapse=',\n'), ',\n',
                                            cols_to_pull,
                                            '\nFROM repeat,',
                                            '\nLATERAL jsonb_to_recordset(element',
                                            .Object@repeats[name==rpt,repeat_relative_path_asjson],
                                            ')\n',
                                            'l_', rpt, '(\"',
                                            paste0(.Object@survey[repeat_parent==rpt & repeat_depth==2,
                                                                  unique(
                                                                    data.table::fifelse(repeat_parent==direct_parent,
                                                                                        paste0(name, '\" text'),
                                                                                        paste0(direct_parent, '\" jsonb')))],
                                              collapse=', \"'),
                                            ')'
                                     )
                                   }))
            # put the view creation command together
            paste0('CREATE OR REPLACE VIEW \"', schema, '\".', view_prefix, view_name,
                   ' AS\n',
                   subquery,
                   paste(mainquery, collapse='\n\nUNION ALL\n\n'),
                   ';')
          })


#' Generate SQL statement
#'
#' Functions to generate SQL statements that create views for all repeats in the ODK form.
#' `genSQL` generates a single statement while `genSQLAll` is a convenience function
#' to generate the statements to create views for all parts of the form
#' (the base statement for questions outside repeats and a view for all repeats or as configured with `combine_views` in [createODK()].
#'
#' @param .Object `XLSForm` object to base the query on.
#' @param schema Name of the schema in which to create the view(s)
#' @param view_name Name of the view to create within `schema`.
#' For repeats this should match the name of the repeat or the `view` column in `combine_views` for [createODK()].
#' Cannot be specified for `genSQLAll`, only for `genSQL`.
#' @param startdate The start date of the period for which forms should be included in this view.
#' If NULL date period has no begin date
#' @param enddate The end date of the period for which forms should be included in this view
#' If NULL date period has no end date
#' @param base Whether this is a call to generate the base query (`TRUE`) or a repeat query (`FALSE`).
#' Can only be specified in `genSQL`, but not in `genSQLAll`
#' @param view_prefix The prefix to add to the view name. Especially useful when creating views for repeats
#' to associate with the relevant form/activity
#' @param filter_version_sql Logical, indicating whether the view should use the form version supplied to filter.
#' If `TRUE`, `form_version` should be specified.
#' @param source_table The table which contains all form submissions from ODK.
#' This function assumes that one of the columns in this table (`as_json`) contains data in JSON format.
#' @param distinct_on Character vector indicating which question names (if any) should be used for
#' a `DISTINCT ON` clause for the view. Useful when the same form has been sent to server multiple times by error.
#' @param sm_exclude Questions with type of `select_multiple` which should not be queried to create
#' separate columns for each possible response, and instead return the text string, possibly contining multiple responses.
#' Intended for use when the options for the question are defined dynamically so the choices sheet cannot contain all options.
#' @aliases genSQL
#' @export

setMethod('genSQL', 'XLSForm', function(.Object, schema, view_name,
                                        startdate=NULL, enddate=NULL,
                                        base=F, view_prefix='',
                                        filter_version_sql=F,
                                        source_table='openhds.form_submission',
                                        distinct_on=NULL, sm_exclude=vector('character', length=0)){
  # add _ after prefix if missing
  if(!substr(view_prefix, nchar(view_prefix), nchar(view_prefix)) %in% c('_', '-') &
        nchar(view_prefix)>1){
    view_prefix=paste0(view_prefix, '_')
  }
  if(base==F){
    if(!view_name %in% .Object@survey[,unique(repeat_view)]){
      stop(paste0("No repeat with name \'", view_name, "\'\nDid you mean to set base=TRUE?"))
    }
    subset_view=subsetView(.Object, view_name)
    maxlev=subset_view@survey[,max(repeat_depth)]
    combined=view_name %in% subset_view@combine_views[,unique(view)]
    if(maxlev==1 & combined==F){
      sql=genSQLRepeatLevel1(.Object, schema=schema, view_name=view_name,
                             startdate=startdate, enddate=enddate, view_prefix=view_prefix,
                             filter_version_sql=filter_version_sql,
                             source_table=source_table, distinct_on=distinct_on,
                             sm_exclude=sm_exclude)
    } else if(maxlev==1 & combined==T){
      sql=genSQLRepeatLevel1_combine(.Object, schema=schema, view_name=view_name,
                                     startdate=startdate, enddate=enddate, view_prefix=view_prefix,
                                     filter_version_sql=filter_version_sql,
                                     source_table=source_table, distinct_on=distinct_on,
                                     sm_exclude=sm_exclude)
    } else if (maxlev==2) {
      sql=genSQLRepeatLevel2(.Object, schema=schema, view_name=view_name,
                             startdate=startdate, enddate=enddate, view_prefix=view_prefix,
                             filter_version_sql=filter_version_sql,
                             source_table=source_table, distinct_on=distinct_on)
    }

  } else{
    if(view_name %in% .Object@survey[,unique(repeat_view)]){
      stop(paste0("View name \'", view_name, "\' is a view for a repeat\nDid you mean to set base=FALSE?"))
    }
    sql=genSQLBase(.Object, schema=schema, view_name=view_name,
                   startdate=startdate, enddate=enddate, view_prefix=view_prefix,
                   filter_version_sql=filter_version_sql,
                   source_table=source_table, distinct_on=distinct_on,
                   sm_exclude=sm_exclude)
  }
})

#' @aliases genSQL
#' @aliases genSQLAll
#' @param base_view_name Name the view that should be created for the base statement
#' (ie. questions not in a repeat)
#' @rdname genSQL-XLSForm-method
#' @export

setMethod('genSQLAll', 'XLSForm', function(.Object, schema, view_name, startdate, enddate,
                                           base_view_name, view_prefix='',
                                           filter_version_sql=F,
                                           source_table='openhds.form_submission',
                                           distinct_on=NULL,
                                           sm_exclude=vector('character', length=0)){
  repeats=.Object@survey[repeat_depth>0,unique(repeat_view)]
  if(NA %in% repeats){
    stop('Not all fields in repeat have a defined repeat_view')
  }
  base_sql=genSQL(.Object, schema=schema, view_name=base_view_name,
                  startdate=startdate, enddate=enddate, view_prefix=view_prefix,
                  filter_version_sql=filter_version_sql,
                  source_table=source_table, distinct_on=distinct_on,
                  sm_exclude=sm_exclude, base=T)
  repeat_sql=sapply(repeats, function(rpt){
    genSQL(.Object, schema=schema, view_name=rpt,
           startdate=startdate, enddate=enddate, view_prefix=view_prefix,
           filter_version_sql=filter_version_sql,
           source_table=source_table, distinct_on=distinct_on,
           sm_exclude=sm_exclude, base=F)
  })
  c(base_sql, repeat_sql)
})

setMethod('genFullViewNames', 'XLSForm', function(.Object, schema, base_name, view_prefix=''){
  paste0('\"', schema, '\".', view_prefix,
         ifelse(!substr(view_prefix, nchar(view_prefix), nchar(view_prefix)) %in% c('_', '-') &
                  nchar(view_prefix)>1,
                '_', ''),
         c(base_name, .Object@survey[!is.na(repeat_view),unique(repeat_view)]))
})

#' Generate query to change ownership of views
#'
#' This function generates a vector of SQL statements which change the ownership of the views defined by an XLSForm.
#' It should be called after generating SQL statements with [genSQL()] and/or [genSQLAll()]
#' and *executing* these queries to create the views in the database.
#'
#' @param .Object The XLSForm object to use for creating the queries
#' @param schema The schema in which views were created
#' @param base_name Name of the base view as created
#' @param owner What should the new owner of these views be (must be a user/group in the PostGreSQL database)
#' @param view_prefix The prefix added to view names as created
#'
#' @aliases genPermissionsSQL
#' @export

setMethod('genPermissionsSQL', 'XLSForm', function(.Object, schema, base_name, owner, view_prefix=''){
  views=genFullViewNames(.Object, schema=schema, base_name=base_name, view_prefix=view_prefix)
  sql=paste('ALTER TABLE', views, 'OWNER TO', owner)
})




#######################################
# Generate a codebook

#' Generate codebook
#'
#' Generate a codebook based on an ODK Form.
#'
#' @param .Object The XLSForm object
#' @param lang Language to use for creating the codebook.
#' Should be a two-letter language code used in the form to define labels/hints for different languages (e.g. EN for english)
#' @param baseviewname Name of the base view to use in the codebook (questions are organized by view).
#' Also used to identify questions by view, so must be equal to baseviewname as specified previously
#' (e.g. in `combine_views` argument to [create_odk()])
#' @param view_prefix Prefix for the view name for labelling sections of the codebook
#' @param exclude Character vector of questions to exclude from the codebook
#' @param replace_choices Character vector mapping question names to strings which should be used in the codebook
#' instead of the options listed in the XLSForm. For example, useful when options are defined dynamically.
#' @param outfile File where the codebook should saved (must be a .txt file)
#' @aliases genCodebook
#' @export

setMethod('genCodebook', 'XLSForm', function(.Object, lang='', includeCalculate=T,
                                             baseviewname='', view_prefix='',
                                             exclude=vector('character', 0),
                                             replace_choices=vector('character', 0),
                                             outfile=NULL){
  if(lang=='') {
    if(!'label' %in% colnames(.Object@survey)){
      stop('XLSForm does not contain column \'label\' and no language specified')
    }
    labelcol='label'
  } else{
    labelcol=colnames(.Object@survey)[grepl('label', colnames(.Object@survey)) &
                                        grepl(paste0('\\(', lang, '\\)'), colnames(.Object@survey))]
    if(length(labelcol)==0){
      stop(paste0('No label column matching language (', lang, ').\n',
                  'Column name should be in form \'label::[Name] ([code])\', e.g. label::English (en)'))
    }
  }
  # update baseviewname if empty
  baseviewname=ifelse(baseviewname=='', 'base', baseviewname)
  # put a underscore on prefix if not ends with _ or -
  if(!substr(view_prefix, nchar(view_prefix), nchar(view_prefix)) %in% c('_', '-') &
     nchar(view_prefix)>0){
    view_prefix=paste0(view_prefix, '_')
  }

  # define a column in the choice if a dictionary is needed (don't repeat names and labels if they're the same)
  .Object@choices[,includeDict:=any(name!=get(labelcol)), by=list_name]


  codebook=.Object@survey[,.(name, newname, type, path, label=get(labelcol), choicelist, select_multiple)]
  # create a better path for codebook (using / instead of ->)
  codebook[,path:=gsub('\\s*->*\\s*', '/', gsub(' as_json -> |[\'\"]', '', path))]
  # remove span tags
  codebook[,label:=gsub("<span[^<]*>|</span>", "", label)]
  # replace odk variables (e.g. ${Name})
  varpat='.*\\$\\{([^\\}]*)\\}.*' # looks for ${}
  odk_vars=unique(gsub(varpat, '\\1', codebook[grepl(varpat, label),label]))
  path_vars=sapply(odk_vars, function(var) codebook[name==var,path])
  for(i in 1:length(odk_vars)){
    codebook[,label:=gsub(paste0('\\$\\{', odk_vars[i], '\\}'),
                          paste0('(', path_vars[i], ')'), label)]
  }
  views=c(baseviewname, .Object@survey[!is.na(repeat_view),unique(repeat_view)])
  if(length(views)==1){ # everything in one view (no repeats)
    entries=apply(codebook[!name %in% exclude], 1, function(row){
      choices=.Object@choices[list_name==row['choicelist']]
      paste0(row['newname'], ' (/', row['path'], '): ', strsplit(row['type'], ' ')[[1]][1], '\n',
             ifelse(row['name']=='instanceID', 'Form unique ID',
                    row['label']), '\n',
             data.table::fcase(row['newname'] %in% names(replace_choices),
                   paste0('\t', replace_choices[row['newname']]),
                   !is.na(row['choicelist']),
                   paste(
                     apply(choices, 1, function(crow){
                       ifelse(crow['includeDict']==T,
                              paste0('\t', crow['name'], ':\t', crow[labelcol]),
                              paste0('\t', crow['name']))
                     }),
                     collapse='\n'),
                   default='')
      )
    })
  } else{ # we have repeats and multiple views to deal with
    entries_views=sapply(views, function(v){
        if(v==baseviewname){
          viewcols=.Object@survey[include_base==T &
                                  !newname %in% exclude,unique(newname)]
        } else{
          viewcols=.Object@survey[get(paste0('include_', v))==T &
                                    !newname %in% exclude,unique(newname)]
        }
        entries_view=paste(
          sapply(viewcols, function(col){
            mappedcols=codebook[newname==col]
              ifelse(mappedcols[,.N]>1,
                     # multiple nodes (questions) mapped to same column in the view
                     paste0(col, ' -- Contains the following ', mappedcols[,.N], ' questions:\n\n',
                            paste(
                              sapply(1:nrow(mappedcols), function(i){
                                row=unlist(mappedcols[i])
                                paste0(i, '.\t', row['name'], ' (/', row['path'], '): ', strsplit(row['type'], ' ')[[1]][1], '\n',
                                       ifelse(row['name']=='instanceID', '\tForm unique ID',
                                              paste0('\t', row['label'])),
                                       ifelse(!is.na(row['choicelist']) |
                                                       row['name'] %in% replace_choices, '\n', ''),
                                       data.table::fcase(row['newname'] %in% names(replace_choices),
                                             paste0('\t\t', replace_choices[row['newname']]),
                                             !is.na(row['choicelist']) &
                                               .Object@choices[list_name==row['choicelist'],any(name!=get(labelcol))],
                                             paste(
                                               apply(.Object@choices[list_name==row['choicelist']], 1, function(crow){
                                                 paste0('\t\t', crow['name'], ':\t', crow[labelcol])
                                               }),
                                               collapse='\n'),
                                             !is.na(row['choicelist']),
                                             paste(paste0('\t',
                                                          .Object@choices[list_name==row['choicelist'],name]),
                                                   collapse='\n'),
                                             default=''))
                                }),
                              collapse='\n\n')
                            ),
                     # only one node mapped to the column in the view
                     paste0(mappedcols[,newname], ' (/', mappedcols[,path], '): ', strsplit(mappedcols[,type], ' ')[[1]][1], '\n',
                            ifelse(mappedcols[,name]=='instanceID', 'Form unique ID',
                                   mappedcols[,label]),
                            ifelse(!is.na(mappedcols[,choicelist]) |
                                     mappedcols[,newname] %in% replace_choices, '\n', ''),
                            data.table::fcase(mappedcols[,newname] %in% names(replace_choices),
                                  paste0('\t', replace_choices[mappedcols[,newname]]),
                                  !is.na(mappedcols[,choicelist])  &
                                    .Object@choices[list_name==mappedcols[,choicelist],any(name!=get(labelcol))],
                                  paste(
                                    apply(.Object@choices[list_name==mappedcols[,choicelist]], 1, function(crow){
                                      paste0('\t', crow['name'], ':\t', crow[labelcol])
                                    }),
                                    collapse='\n'),
                                  !is.na(mappedcols[,choicelist]),
                                  paste(paste0('\t',
                                               .Object@choices[list_name==mappedcols[,choicelist],name]),
                                        collapse='\n'),
                                  default=''))
                     )}),
          collapse='\n\n')
        paste0('===================================================================\n',
               'VIEW: \n',
               view_prefix, v,
               '\n===================================================================\n\n',
               entries_view)
        })
    entries=entries_views
  }



  codebook_txt=paste0('QUESTION TYPES\n\n',
                      'select_one:\t\tselect one option\n',
                      'select_multiple:\tselect one or more options\n',
                      'int:\t\t\tEnter a whole number (not allowing decimal places)\n',
                      'decimal:\t\tEnter a number (allowing decimal places)\n',
                      'date:\t\t\tEnter a date\n',
                      'text:\t\t\tFree text\n',
                      'calculate:\t\tCalculated field using previous questions\n',
                      'hidden:\t\t\tAutomatically populated by CIMS\n\n',
                      'FORM QUESTIONS\n\n',
                      paste(entries, collapse='\n\n\n'))
  #codebook_txt=enc2utf8(codebook_txt)
  if(!is.null(outfile)) cat(iconv(codebook_txt, to='UTF-8'), file=outfile)
  invisible(codebook_txt)
})

