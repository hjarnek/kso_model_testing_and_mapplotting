# -*- coding: utf-8 -*-

from qgis.PyQt.QtCore import QCoreApplication, QVariant
from qgis.core import (QgsProcessing,
                       QgsProcessingException,
                       QgsProcessingAlgorithm,
                       QgsProcessingParameterFeatureSource,
                       QgsProcessingParameterFeatureSink,
                       QgsProcessingParameterField,
                       QgsField,
                       QgsGeometry,
                       QgsExpression,
                       QgsExpressionContext,
                       QgsExpressionContextUtils,
                       edit,
                       QgsEditError,
                       QgsFeatureRequest,
                       QgsProject)
from qgis import processing


class GridTimes(QgsProcessingAlgorithm):

    # Constants used to refer to parameters and outputs.
    INPUT = 'INPUT'
    TIMESTAMPS = 'TIMESTAMPS'
    GRID = 'GRID'
    OUTPUT = 'OUTPUT'

    def tr(self, string):
        """
        Returns a translatable string with the self.tr() function.
        """
        return QCoreApplication.translate('Processing', string)

    def createInstance(self):
        return GridTimes()

    def name(self):
        """
        Returns the algorithm name, used for identifying the algorithm. This
        string should be fixed for the algorithm, and must not be localised.
        The name should be unique within each provider. Names should contain
        lowercase alphanumeric characters only and no spaces or other
        formatting characters.
        """
        return 'gridtimes'

    def displayName(self):
        """
        Returns the translated algorithm name, which should be used for any
        user-visible display of the algorithm name.
        """
        return self.tr('Grid times')

    def group(self):
        """
        Returns the name of the group this algorithm belongs to. This string
        should be localised.
        """
        return self.tr('Route processing')

    def groupId(self):
        """
        Returns the unique ID of the group this algorithm belongs to. This
        string should be fixed for the algorithm, and must not be localised.
        The group id should be unique within each provider. Group id should
        contain lowercase alphanumeric characters only and no spaces or other
        formatting characters.
        """
        return 'routeprocessing'

    def shortHelpString(self):
        """
        Returns a localised short helper string for the algorithm. This string
        should provide a basic description about what the algorithm does and the
        parameters and outputs associated with it.
        """
        return self.tr("Take an input layer of waypoints and create a timed route polyline layer where the length of each line segment is determined by the lines of a grid overlay, and enter/exit times for every crossed grid cell are calculated.")

    def helpUrl(self):
        return "https://github.com/hjarnek/kso_model_testing_and_mapplotting/tree/main/geoprocessing"

    def AddField(self, layer, field, fieldtype, expr):
        expression = QgsExpression(expr)
        context = QgsExpressionContext()
        context.appendScopes(QgsExpressionContextUtils.globalProjectLayerScopes(layer))
        try:
            with edit(layer):
                layer.dataProvider().addAttributes([QgsField(field, fieldtype)])
                layer.updateFields()
                for f in layer.getFeatures():
                    context.setFeature(f)
                    f[field] = expression.evaluate(context)
                    layer.updateFeature(f)
        except QgsEditError as err:
            print(repr(err))

    def JoinMultipleLines(self, geom, queue_list):
        """
        This section is contains an embedded version of the Join Multiple Lines
        QGIS plugin. Credits go to its author Daan Goedkoop.
        Plugin homepage: https://github.com/dgoedkoop/joinmultiplelines
        """
        if geom is None:
            if len(queue_list) > 0:
                return queue_list.pop()
            else:
                return None
        base_firstvertex = geom.vertexAt(0)
        base_lastvertex = geom.vertexAt(len(geom.asPolyline()) - 1)
        found_geom = None
        found_distance = 0
        for i_geom in queue_list:
            i_firstvertex = i_geom.vertexAt(0)
            i_lastvertex = i_geom.vertexAt(len(i_geom.asPolyline()) - 1)
            distance_baselast_ifirst = base_lastvertex.distanceSquared(i_firstvertex)
            distance_baselast_ilast = base_lastvertex.distanceSquared(i_lastvertex)
            distance_basefirst_ifirst = base_firstvertex.distanceSquared(i_firstvertex)
            distance_basefirst_ilast = base_firstvertex.distanceSquared(i_lastvertex)
            distance = distance_baselast_ifirst
            base_reverse = False
            i_reverse = False
            if distance_baselast_ilast < distance:
                distance = distance_baselast_ilast
                base_reverse = False
                i_reverse = True
            if distance_basefirst_ifirst < distance:
                distance = distance_basefirst_ifirst
                base_reverse = True
                i_reverse = False
            if distance_basefirst_ilast < distance:
                distance = distance_basefirst_ilast
                base_reverse = True
                i_reverse = True
            if (found_geom is None) or (distance < found_distance):
                found_geom = i_geom
                found_distance = distance
                found_base_reverse = base_reverse
                found_i_reverse = i_reverse
        if found_geom is not None:
            queue_list.remove(found_geom)
            geom_line = geom.constGet()
            found_geom_line = found_geom.constGet()
            if found_base_reverse:
                geom_line = geom_line.reversed()
            if found_i_reverse:
                found_geom_line = found_geom_line.reversed()
            geom_line.append(found_geom_line)
            geom.set(geom_line)
            return geom
        else:
            return None


    def initAlgorithm(self, config=None):
        """
        Here we define the inputs and output of the algorithm, along
        with some other properties.
        """
        
        self.addParameter(
            QgsProcessingParameterFeatureSource(
                self.INPUT,
                self.tr('Input point layer'),
                types=[QgsProcessing.TypeVectorPoint]
            )
        )
        
        self.addParameter(
            QgsProcessingParameterField(
                self.TIMESTAMPS,
                self.tr('Timestamp field'),
                parentLayerParameterName = self.INPUT,
                defaultValue = 'UNIXTime'
            )
        )
        
        self.addParameter(
            QgsProcessingParameterFeatureSource(
                self.GRID,
                self.tr('Grid polygon layer'),
                types=[QgsProcessing.TypeVectorPolygon]
            )
        )
        
        self.addParameter(
            QgsProcessingParameterFeatureSink(
                self.OUTPUT,
                self.tr('Output layer')
            )
        )


    def processAlgorithm(self, parameters, context, feedback):
        """
        Here is where the processing itself takes place.
        """
        
        # If any of the required inputs are missing, throw an exception to
        # indicate that the algorithm encountered a fatal error.
        if self.INPUT is None:
            raise QgsProcessingException(self.invalidSourceError(parameters, self.INPUT))
        if self.TIMESTAMPS is None:
            raise QgsProcessingException(self.invalidSourceError(parameters, self.TIMESTAMPS))
        if self.GRID is None:
            raise QgsProcessingException(self.invalidSourceError(parameters, self.GRID))
        
        # Retrieve the feature source.
        source = self.parameterAsVectorLayer(parameters, self.INPUT, context)
        layername = source.id()
        fieldname = parameters[self.TIMESTAMPS]
        source_first_fid = next(source.getFeatures()).id()
        if (source_first_fid != 0) and (source_first_fid != 1):
            raise QgsProcessingException('ERROR: The first feature ID of the input \
            point layer is neither 0 nor 1. Must be either to continue.')
        
        # Send some information to the user
        feedback.pushInfo('CRS is {}'.format(source.sourceCrs().authid()))
        feedback.pushInfo('Number of points: {}'.format(source.featureCount()))
        
        layer1_line = processing.run("native:pointstopath", {
            'INPUT':parameters[self.INPUT],
            'CLOSE_PATH':False,
            'ORDER_EXPRESSION':fieldname,
            'NATURAL_SORT':False,
            'GROUP_EXPRESSION':'',
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer2_exploded = processing.run("native:explodelines", {
            'INPUT':layer1_line,
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer2_exploded_layerobject = context.getMapLayer(layer2_exploded)
        first_fid = next(layer2_exploded_layerobject.getFeatures()).id()
        diff = source_first_fid - first_fid
        expr_enter = 'attributes(get_feature_by_id(\'{}\', ($id+{})))[\'{}\']'\
        .format(layername, diff, fieldname)
        expr_speed = '$length / ((attributes(get_feature_by_id(\'{}\', ($id+1+{})\
        ))[\'{}\']) - (attributes(get_feature_by_id(\'{}\', ($id+{})))[\'{}\']))'\
        .format(layername, diff, fieldname, layername, diff, fieldname)
        
        self.AddField(layer2_exploded_layerobject, 'enter2', QVariant.Double, expr_enter)
        self.AddField(layer2_exploded_layerobject, 'speed', QVariant.Double, expr_speed)

        f1id = layer2_exploded_layerobject.fields().lookupField('begin')
        f2id = layer2_exploded_layerobject.fields().lookupField('end')
        layer2_exploded_layerobject.dataProvider().deleteAttributes([f1id, f2id])
        layer2_exploded_layerobject.updateFields()
        
        grid_line_layer = processing.run("native:polygonstolines", {
            'INPUT':parameters[self.GRID],
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer3_split = processing.run("native:splitwithlines", {
            'INPUT':layer2_exploded,
            'LINES':grid_line_layer,
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer3_split_layerobject = context.getMapLayer(layer3_split)
        field_names = ['enter2', 'speed']
        field_idxs = [layer3_split_layerobject.fields().indexFromName(field) for field in field_names]
        request = QgsFeatureRequest().setSubsetOfAttributes(field_idxs)
        layer3_split_fields = layer3_split_layerobject.materialize(QgsFeatureRequest(request))
        layer3_split_fields.setName('Fields backup')
        QgsProject.instance().addMapLayer(layer3_split_fields)
        
        layer3_split_layerobject.selectAll()
        allfeats = layer3_split_layerobject.selectedFeatures()
        geomlist = []
        for feat in allfeats:
            geom = QgsGeometry(feat.geometry())
            if geom.isMultipart():
                for geom_i in geom.asGeometryCollection():
                    geomlist.append(geom_i)
            else:
                geomlist.append(geom)
        newgeom = None
        while len(geomlist) > 0:
            newgeom = self.JoinMultipleLines(newgeom, geomlist)
        layer3_split_layerobject.startEditing()
        layer3_split_layerobject.changeGeometry( allfeats[0].id(), newgeom )
        for feat in allfeats:
            if feat != allfeats[0]:
                layer3_split_layerobject.deleteFeature( feat.id() )
                
        layer4_exploded = processing.run("native:explodelines", {
            'INPUT':layer3_split,
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        layer4_exploded_layerobject = context.getMapLayer(layer4_exploded)
        
        starting_point = QgsGeometry(source.getFeature(source_first_fid).geometry()).vertexAt(0)
        starting_point_layer4 = layer4_exploded_layerobject.getFeature(1).geometry().constGet()[0]
        if (starting_point_layer4 != starting_point):
            layer4_exploded = processing.run("native:orderbyexpression", {
                'INPUT':layer4_exploded,
                'EXPRESSION':'$id',
                'ASCENDING':False,
                'NULLS_FIRST':False,
                'OUTPUT':'TEMPORARY_OUTPUT'
            }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
            layer4_exploded_layerobject = context.getMapLayer(layer4_exploded)
            try:
                with edit(layer4_exploded_layerobject):
                    for feature in layer4_exploded_layerobject.getFeatures():
                        geom = feature.geometry()
                        nodes = geom.asPolyline()
                        nodes.reverse()
                        newgeom = QgsGeometry.fromPolylineXY(nodes)
                        layer4_exploded_layerobject.changeGeometry(feature.id(),newgeom)
            except QgsEditError as err:
                print(repr(err))
            starting_point_layer4 = layer4_exploded_layerobject.getFeature(1).geometry().constGet()[0]
            if (starting_point_layer4 != starting_point):
                raise QgsProcessingException('ERROR: Something went wrong ordering the \
                line segments in connection with JoinMultipleLines')
        
        expr_enter2 = 'attributes(get_feature_by_id(\'{}\', $id))[\'enter2\']'\
        .format(layer3_split_fields.id())
        expr_speed2 = 'attributes(get_feature_by_id(\'{}\', $id))[\'speed\']'\
        .format(layer3_split_fields.id())
        
        self.AddField(layer4_exploded_layerobject, 'enter2', QVariant.Double, expr_enter2)
        self.AddField(layer4_exploded_layerobject, 'speed', QVariant.Double, expr_speed2)
        
        QgsProject.instance().removeMapLayer(layer3_split_fields.id())
        
        expr_enter3 = '\"enter2\" + if((sum($length, group_by:=\"enter2\", filter:=\
        ($id<=@row_number))) IS NULL, 0, ((sum($length, group_by:=\"enter2\", filter:=\
        ($id<=@row_number))) / \"speed\"))'
        expr_exit3 = '\"enter2\" + ((sum($length, group_by:=\"enter2\", filter:=($id<=(@row_number+1)))) / \"speed\")'
        
        layer5_fieldcalc = processing.run("native:fieldcalculator", {
            'INPUT':layer4_exploded,
            'FIELD_NAME':'enter',
            'FIELD_TYPE':0,
            'FIELD_LENGTH':0,
            'FIELD_PRECISION':0,
            'FORMULA':expr_enter3,
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer6_fieldcalc = processing.run("native:fieldcalculator", {
            'INPUT':layer5_fieldcalc,
            'FIELD_NAME':'exit',
            'FIELD_TYPE':0,
            'FIELD_LENGTH':0,
            'FIELD_PRECISION':0,
            'FORMULA':expr_exit3,
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer6_joined = processing.run("native:joinattributesbylocation", {
            'INPUT':layer6_fieldcalc,
            'JOIN':parameters[self.GRID],
            'PREDICATE':[0],
            'JOIN_FIELDS':['id'],
            'METHOD':2,
            'DISCARD_NONMATCHING':False,
            'PREFIX':'',
            'OUTPUT':parameters[self.OUTPUT]
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer6_joined_layerobject = context.getMapLayer(layer6_joined)
        f1id = layer6_joined_layerobject.fields().lookupField('enter2')
        layer6_joined_layerobject.dataProvider().deleteAttributes([f1id])
        layer6_joined_layerobject.updateFields()
        
        return {self.OUTPUT: layer6_joined}
